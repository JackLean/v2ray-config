#! /usr/bin/env ruby

QUALIFIED_NAME = /\A[-a-z0-9]+\.[-.a-z0-9]+\z/
def translate_host_pattern(pat)
  v2pat = case
          when pat.start_with?('full:')   then pat
          when pat.start_with?('domain:') then pat
          when pat.start_with?('regexp:') then pat
          when pat.start_with?(':')       then nil
          when pat =~ %r{\A[^/].*:}       then nil
          when pat =~ QUALIFIED_NAME            then "full:#{pat}"
          when (pat.start_with?('+ ') &&
                pat[2..-1] =~ QUALIFIED_NAME)   then "domain:#{pat[2..-1]}"
          when pat =~ %r{\A/[^/]*/\z}           then "regexp:#{pat[1..-2]}"
          when pat =~ %r{\A//[^/]*//\z}         then "regexp:#{translate_regex_dialect(pat[2..-3])}"
          when pat =~ %r{\A/.*/\z}m             then nil    # //abc/ or /abc// or ///abc///
          else nil
          end
  v2pat or abort "Invalid host pattern: #{pat.inspect}"
  v2pat
end



def translate_regex_dialect(dialect_pat)       # RegexDialect has too many methods.  This function makes it clear which method is the entry point.
  RegexDialect.translate(dialect_pat)
end

module RegexDialect
  def self.translate(dialect_pat)
    syn_tree = self.analyze(dialect_pat)
    trad_pat = self.synthesize(syn_tree)
    if syn_tree.size > 1
      trad_pat = '(' + trad_pat + ')'
    end
    "^#{trad_pat}$"
  end

  #
  # Syntax Tree format
  #
  # Pattern:           [option1, option2, ...]            n >= 1
  # PatternNoAlt:      [atom1, atom2, ...]                n >= 0
  #
  # Atom:              LiteralChar | AnyChar | CharClass | Repetition | ZeroWidth | Pattern
  # Repetition:        [:repeat, int, int/nil, Atom]                  nil means unlimited times of repetition           example: //,{3,5}// is converted to [:repeat, 3, 5, :anychar]
  #
  # LiteralChar:       'a'
  # AnyChar:           :anychar
  # CharClass:         [:charclass, true/false, classitem1, classitem2, ...]     n >= 1.    true indicates negation        if dash(-) is an item, it must be the last
  #    CharClassItem:  ['a', 'f']                 //[x]// is expanded to ['x', 'x']   //[A-Z]// is converted to ['a', 'z']      if begin != end, then both must be lowercase letter or both must be digits
  #
  # ZeroWidth:         :wordboundary | :notwordboundary
  #

  # argument must be a Pattern, not any other node
  def self.synthesize(syntax_tree)
    self.synthesize_alternation(syntax_tree)
  end

  def self.synthesize_alternation(options)
    options
      .map {|x| self.synthesize_atomlist(x) }
      .join('|')
  end

  def self.synthesize_atomlist(atoms)
    atoms
      .map {|x| self.synthesize_atom(x) }
      .join
  end

  def self.synthesize_atom(node)
    case node
    when :anychar           then '.'
    when String             then GoRegex.regex_for_literal_char(node)
    when :wordboundary      then '\b'
    when :notwordboundary   then '\B'
    else
      case node[0]
      when :charclass       then self.synthesize_charclass(*node)
      when :repeat          then self.synthesize_repetition(*node)
      when Array            then '(' + self.synthesize_alternation(node) + ')'
      end
    end
  end

  # As mentioned above, if "-" is an item, it must be the last item
  def self.synthesize_charclass(ignore, negative, *items)
    body = items
      .map {|x,y| x == y ? x : "#{x}-#{y}" }
      .join
    pat = negative ? "[^#{body}]" : "[#{body}]"
    pat
  end

  def self.synthesize_repetition(ignore, min_rep, max_rep, atom)
    root = self.synthesize_atom(atom)
    # disambiguate nested repetition
    if atom.class == Array  && atom[0] == :repeat   # a+? may mean (a+)?, not lazy a+
      root = '(' + root + ')'
    end

    mod = if min_rep == 0 && max_rep == nil
            '*'
          elsif min_rep == 1 && max_rep == nil
            '+'
          elsif max_rep == nil
            "{#{min_rep},}"
          elsif min_rep == 0 && max_rep == 1
            '?'
          elsif min_rep == max_rep
            "{#{min_rep}}"
          else
            "{#{min_rep},#{max_rep}}"
          end

    root + mod
  end






  def self.analyze(dialect_pat)
    invalid_chars = '#/@`<>'.chars & (dialect_pat.each_char.uniq)
    if invalid_chars.size > 0
      raise 'Invalid AltRegex Pattern: ' + invalid_chars.join(' or ') + 'must not be part of a pattern'
    end

    #
    # Instead of parsing a full item into a final node, we parse naively and adjust nodes as we notice mistakes.
    # For example, in //a*?bc// we do not recognize //a*?// immediately.  Instead, we parse //a// into a LiteralChar.
    # Then we see //*// and replace the LiteralChar with a Repetition.  Then we see //?// and replace the Repetition
    # with a nested Repetition.
    #
    pat_stack = []
    altern = [];      pat_stack << altern
    concat = [];      altern << concat
    mode = :normal    #  :normal | :escape | :classbody | :classend | :repeat

    for c in dialect_pat.each_char
      case mode

      when :normal
        case
        when self.is_space?(c)    then :pass
        when /[-_0-9a-z]/ =~ c    then concat << c
        when /[A-Z]/ =~ c         then concat << c.downcase
        when ".$!\"%&';=~".include?(c)     then concat << c
        when c == ','             then concat << :anychar
        when c == ':'             then concat << [:charclass, true, ['.', '.']]

        when c == '\\'            then mode = :escape

        when '*+?'.include?(c)
          if concat.empty?
            raise "Invalid AltRegex Pattern: \"#{c}\" must be preceded by something (input pattern: #{dialect_pat.inspect})"
          end
          min_rep, max_rep = case c
                             when '*' then [0, nil]
                             when '+' then [1, nil]
                             when '?' then [0, 1]
                             end
          atom = concat[-1]
          concat[-1] = [:repeat, min_rep, max_rep, atom]

        when c == ']'             then raise "Invalid AltRegex Pattern: unmatched closing bracket (#{dialect_pat.inspect})"
        when c == '}'             then raise "Invalid AltRegex Pattern: unmatched closing curly bracket (input pattern: #{dialect_pat.inspect})"
        when c == '['             then mode = :classbody
                                       class_members = []        # members could be [x, nil] or [x, y].  nils will be replaced when char class is completed
                                       class_allowdash = false
                                       class_negative = false
                                       class_incomplete_range = false
        when c == '{'             then if concat.empty?
                                         raise "Invalid AltRegex Pattern: {n,m} must be preceded by something (input pattern: #{dialect_pat.inspect})"
                                       end
                                       mode = :repeat
                                       saw_comma = false
                                       num0 = nil
                                       num1 = nil

        when c == '|'             then concat = []; altern << concat
        when c == '('             then concat << [[]]; altern = concat[-1]; concat = altern[0]; pat_stack << altern
        when c == ')'             then pat_stack.pop
                                       if pat_stack.empty? then raise "Invalid AltRegex Pattern: unmatched closing parenthesis (input pattern: #{dialect_pat.inspect})" end
                                       altern = pat_stack[-1]; concat = altern[-1]

        when c == '^'             then raise "Invalid AltRegex Pattern: ^ is not a valid character except in [^abc]"
        else raise "Invalid AltRegex Pattern: #{c.inspect} is not a valid character"    # control character or non-ASCII
        end

      when :escape
        mode = :normal
        case c
        when /[,*+()]/  then concat << c
        when 'd'        then concat << [:charclass, false, ['0', '9']]
        when 'D'        then concat << [:charclass, true,  ['0', '9']]
        when 'w'        then concat << [:charclass, false, ['0', '9'], ['a', 'z'], ['_', '_']]
        when 'W'        then concat << [:charclass, true,  ['0', '9'], ['a', 'z'], ['_', '_']]
        when 'b'        then concat << :wordboundary
        when 'B'        then concat << :notwordboundary
        else raise "Invalid AltRegex Pattern: invalid escape character #{c.inspect}"
        end

      when :classbody
        no_member = class_members.empty? && !class_allowdash
        case c
        when ']'
          if no_member # []
            raise 'Invalid AltRegex Pattern: empty character class ' + (class_negative ? '[^]' : '[]')
          elsif class_incomplete_range # [abcd-] or [-abcd-]
            class_allowdash = true
          end

          concat << self.make_charclass_node(class_negative, class_allowdash, class_members)
          class_members = class_allowdash = class_negative = class_incomplete_range = nil
          mode = :normal

        when '['
          raise "Invalid AltRegex Pattern: nested character class (input pattern: #{dialect_pat.inspect})"
        when /[:?|{}\\]/
          raise "Invalid AltRegex Pattern: \"#{c}\" is not valid inside []"

        when '^'
          if !no_member
            raise "Invalid AltRegex Pattern: ^ is not a valid character class element (input pattern: #{dialect_pat.inspect})"
          elsif class_negative
            raise "Invalid AltRegex Pattern: [^^...]"
          else
            class_negative = true
          end

        when '-'
          if no_member
            class_allowdash = true
          elsif class_members.empty? # [--
            mode = :classend
          elsif class_incomplete_range # [a--
            raise "Invalid AltRegex Pattern: [...--...]"
          elsif class_members[-1][1] == nil # [abc-
            class_incomplete_range = true
          else # [a-f- or [.-
            class_allowdash = true
            mode = :classend
          end

        when /[.,*+$()!"%&';=~_]/
          if class_incomplete_range
            raise "Invalid AltRegex Pattern: invalid range in character class (input pattern: #{dialect_pat.inspect})"
          end
          class_members << [c, c]

        when /[0-9a-zA-Z]/
          c = c.downcase
          if class_incomplete_range
            range = class_members[-1]
            sametype = !!(range[0] =~ /[0-9]/) == !!(c =~ /[0-9]/)
            if sametype && range[0].ord <= c.ord
              range[1] = c
              class_incomplete_range = false
            else
              raise "Invalid AltRegex Pattern: invalid range in character class (input pattern: #{dialect_pat.inspect})"
            end
          else
            class_members << [c, nil]
          end

        else
          if self.is_space?(c)
            :pass
          else
            raise "Invalid AltRegex Pattern: #{c.inspect} is not a valid character"    # control character or non-ASCII
          end
        end

      when :classend
        if self.is_space?(c)
          :pass
        elsif c == ']'
          concat << self.make_charclass_node(class_negative, class_allowdash, class_members)
          class_members = class_allowdash = class_negative = class_incomplete_range = nil
          mode = :normal
        else
          raise "Invalid AltRegex Pattern: inside [], dash(-) must be part of a range or appear at the beginning or the end (input pattern: #{dialect_pat.inspect})"
        end

      when :repeat
        if /[0-9]/ =~ c
          num1 = 0 if num1 == nil
          num1 = num1 * 10 + (c.ord - '0'.ord)
        elsif c == ','
          if saw_comma
            raise "Invalid AltRegex Pattern: {} must not contain more than 1 commas (input pattern: #{dialect_pat.inspect})"
          end
          saw_comma = true
          num0, num1 = num1, nil
        elsif c == '}'
          min_rep, max_rep = if num0 == nil && num1 == nil
                               raise "Invalid AltRegex Pattern: empty repetition " + (saw_comma ? "{,}" : "{}")
                             elsif num0 != nil # {5,} or {5,8}
                               if num1 != nil && num0 > num1
                                 raise "Invalid AltRegex Pattern: n > m in repetition {n,m} (input pattern: #{dialect_pat.inspect})"
                               end
                               [num0, num1]
                             elsif saw_comma # {,8}
                               [0, num1]
                             else # {8}
                               [num1, num1]
                             end
          atom = concat[-1] # must exist because we have checked before
          concat[-1] = [:repeat, min_rep, max_rep, atom]
          mode = :normal
          saw_comma = num0 = num1 = nil
        else
          raise "Invalid AltRegex Pattern: invalid character inside {n,m} repetition (input pattern: #{dialect_pat.inspect})"
        end
      end
    end

    if mode == :escape
      raise "Invalid AltRegex Pattern: backslash at end of pattern (input pattern: #{dialect_pat.inspect})"
    elsif mode == :repeat
      raise "Invalid AltRegex Pattern: unmatched opening curly bracket (input pattern: #{dialect_pat.inspect})"
    elsif mode != :normal
      raise "Invalid AltRegex Pattern: unmatched opening bracket (input pattern: #{dialect_pat.inspect})"
    elsif pat_stack.size != 1
      raise "Invalid AltRegex Pattern: unmatched opening parenthesis (input pattern: #{dialect_pat.inspect})"
    end
    altern
  end

  # destructive
  def self.make_charclass_node(negative, allow_dash, elements)
    elements.map! {|x,y| [x, y||x] }
    elements << ['-', '-'] if allow_dash
    [:charclass, negative, *elements]
  end

  # true if IS VALID and is space
  def self.is_space?(c)
    " \t\n".include? c
  end
end



# only implements a subset used by our dialect
# example: ^ is not escaped to \^
module GoRegex
  def self.regex_for_literal_char(ch)
    if '.$*+()'.include? ch
      "\\" + ch
    else
      ch
    end
  end
end




module RegexDialectTests
  def self.run_all_tests
    self.test_integration   # This one tests ^abc$
    self.test_synthesis
    self.test_analysis
  end

  def self.test_integration
    raise if RegexDialect.translate('abc') != '^abc$'
    raise if RegexDialect.translate('a|b|c') != '^(a|b|c)$'
    raise if RegexDialect.translate('e:.WIKIPEDIA.org') != '^e[^.]\.wikipedia\.org$'
    raise if RegexDialect.translate("e,+  .  WIKI\\+PEDIA\n  .  org") != '^e.+\.wiki\+pedia\.org$'
  end

  def self.test_synthesis
    raise if RegexDialect.synthesize([['a', 'b', :anychar]]) != 'ab.'
    raise if RegexDialect.synthesize([['a'], ['b'], [:wordboundary]]) != 'a|b|\b'
    raise if RegexDialect.synthesize([['a'], [[['b', 'c'], ['d', 'e']], 'f']]) != 'a|(bc|de)f'

    raise if RegexDialect.synthesize_atom('a') != 'a'
    raise if RegexDialect.synthesize_atom('.') != '\.'
    raise if RegexDialect.synthesize_atom('$') != '\$'
    raise if RegexDialect.synthesize_atom('*') != '\*'
    raise if RegexDialect.synthesize_atom('+') != '\+'
    raise if RegexDialect.synthesize_atom('(') != '\('
    raise if RegexDialect.synthesize_atom(')') != '\)'
    raise if RegexDialect.synthesize_atom(:anychar) != '.'
    raise if RegexDialect.synthesize_atom(:wordboundary) != '\b'
    raise if RegexDialect.synthesize_atom(:notwordboundary) != '\B'
    raise if RegexDialect.synthesize_atom([['a']]) != '(a)'

    raise if RegexDialect.synthesize_atom([:charclass, false, ['.', '.']]) != '[.]'
    raise if RegexDialect.synthesize_atom([:charclass, true, ['a', 'z'], ['3', '3'], ['-', '-']]) != '[^a-z3-]'

    raise if RegexDialect.synthesize_atom([:repeat, 3, 5, 'a']) != 'a{3,5}'
    raise if RegexDialect.synthesize_atom([:repeat, 0, 5, 'a']) != 'a{0,5}'
    raise if RegexDialect.synthesize_atom([:repeat, 3, nil, 'a']) != 'a{3,}'
    raise if RegexDialect.synthesize_atom([:repeat, 1, 1, 'a']) != 'a{1}'
    raise if RegexDialect.synthesize_atom([:repeat, 0, 1, 'a']) != 'a?'
    raise if RegexDialect.synthesize_atom([:repeat, 0, nil, :anychar])      != '.*'
    raise if RegexDialect.synthesize_atom([:repeat, 1, nil, :wordboundary]) != '\b+'

    raise if RegexDialect.synthesize_atom([:repeat, 1, nil, [:charclass, false, ['a','z']]]) != '[a-z]+'
    raise if RegexDialect.synthesize_atom([:repeat, 0, 1, [:repeat, 1, nil, :anychar]]) != '(.+)?'
  end

  def self.test_analysis
    raise if RegexDialect.analyze('') != [[]]
    raise if RegexDialect.analyze('abc') != [['a', 'b', 'c']]
    raise if RegexDialect.analyze('a|b|c') != [['a'], ['b'], ['c']]
    raise if RegexDialect.analyze('a(bc|d)|e') != [['a', [['b', 'c'], ['d']]], ['e']]
    raise if RegexDialect.analyze('(())') != [[[[[[]]]]]]
    self.should_raise("(abc")
    self.should_raise("(ab)c)")

    raise if RegexDialect.analyze("\na b\n \tc\n") != [['a', 'b', 'c']]
    self.should_raise("a\r\nb")

    self.should_raise("\\ *")
    self.should_raise("\\\\")
    self.should_raise("\\.")
    self.should_raise("\\:")
    self.should_raise("\\[")
    self.should_raise("\\]")
    self.should_raise("\\{")
    self.should_raise("\\}")
    self.should_raise("\\|")
    self.should_raise("\\^")
    self.should_raise("\\$")
    self.should_raise("\\?")
    raise if RegexDialect.analyze('\\( \\) \\, \\* \\+') != [['(', ')', ',', '*', '+']]

    raise if RegexDialect.analyze('.,:') != [['.', :anychar, [:charclass, true, ['.','.']]]]
    raise if RegexDialect.analyze('\b\B\d\D') != [[:wordboundary, :notwordboundary, [:charclass, false, ['0','9']], [:charclass, true, ['0','9']]]]
    raise if RegexDialect.analyze('\w\W') != [[[:charclass, false, ['0','9'], ['a','z'], ['_','_']], [:charclass, true, ['0','9'], ['a','z'], ['_','_']]]]
    raise if RegexDialect.analyze('\b{5}') != [[[:repeat, 5, 5, :wordboundary]]]
    self.should_raise("\\A")
    self.should_raise("\\z")
    self.should_raise("\\n")
    self.should_raise("\\1")
    self.should_raise("\\x30")

    self.should_raise('*')
    self.should_raise('+')
    self.should_raise('?')
    self.should_raise('{5}')
    self.should_raise('abc(*def)')
    self.should_raise('abc(+def)')
    self.should_raise('abc(?def)')
    self.should_raise('abc({5}def)')
    raise if RegexDialect.analyze('a*') != [[[:repeat, 0, nil, 'a']]]
    raise if RegexDialect.analyze('a+') != [[[:repeat, 1, nil, 'a']]]
    raise if RegexDialect.analyze('a?') != [[[:repeat, 0, 1, 'a']]]

    self.should_raise('a{}')
    self.should_raise('a{,}')
    self.should_raise('a{10,5}')
    self.should_raise('a{10')
    self.should_raise('a10}')
    raise if RegexDialect.analyze('a{5}') != [[[:repeat, 5, 5, 'a']]]
    raise if RegexDialect.analyze('a{5,}') != [[[:repeat, 5, nil, 'a']]]
    raise if RegexDialect.analyze('a{,5}') != [[[:repeat, 0, 5, 'a']]]
    raise if RegexDialect.analyze('a{3,5}') != [[[:repeat, 3, 5, 'a']]]
    raise if RegexDialect.analyze('a{0}') != [[[:repeat, 0, 0, 'a']]]
    raise if RegexDialect.analyze('a{0000,0000}') != [[[:repeat, 0, 0, 'a']]]  # allow equal, allow zero, and allow leading zeroes
    raise if RegexDialect.analyze('a{0009,0999}') != [[[:repeat, 9, 999, 'a']]]  # leading zero is not octal
    raise if RegexDialect.analyze('a{1001,1002}') != [[[:repeat, 1001, 1002, 'a']]]  # Go does not allow more than 1000 repetitions, but that should be reported by Go, not by us.
    raise if RegexDialect.analyze('a{0,1234567890123456789012}') != [[[:repeat, 0, 1234567890123456789012, 'a']]]  # greater than uint64_t. could be a concern for cross-language portability.
    self.should_raise('a{ 5}')
    self.should_raise('a{5 }')
    self.should_raise('a{5 ,}')
    self.should_raise('a{5, }')
    self.should_raise('a{5,5 }')
    self.should_raise('a{0 0}')

    raise if RegexDialect.analyze(',+?') != [[[:repeat, 0, 1, [:repeat, 1, nil, :anychar]]]]
    raise if RegexDialect.analyze(',?*') != [[[:repeat, 0, nil, [:repeat, 0, 1, :anychar]]]]
    raise if RegexDialect.analyze(',{3}{5}') != [[[:repeat, 5, 5, [:repeat, 3, 3, :anychar]]]]

    # basics
    self.should_raise('[]')
    self.should_raise('[^]')
    self.should_raise('[^^abc]')
    self.should_raise('[abc')
    self.should_raise('[abc-') # whitebox test: classend
    self.should_raise(']')
    raise if RegexDialect.analyze('[^a]') != [[[:charclass, true, ['a','a']]]]
    raise if RegexDialect.analyze('[aa]') != [[[:charclass, false, ['a','a'], ['a','a']]]]
    raise if RegexDialect.analyze('[(]') != [[[:charclass, false, ['(','(']]]]
    raise if RegexDialect.analyze('[)]') != [[[:charclass, false, [')',')']]]]
    raise if RegexDialect.analyze('[aA_]') != [[[:charclass, false, ['a','a'], ['a','a'], ['_','_']]]]
    raise if RegexDialect.analyze('[abc-xyz]') != [[[:charclass, false, ['a','a'], ['b','b'], ['c','x'], ['y', 'y'], ['z','z']]]]
    raise if RegexDialect.analyze('[0-9]') != [[[:charclass, false, ['0','9']]]]

    # caret and brackets
    self.should_raise('[a^]')
    self.should_raise('[a\^]')
    self.should_raise('[abcdef&&[^cd]]')
    self.should_raise('[[:alnum:]]')
    self.should_raise('[\[]')
    self.should_raise('[\]]')
    self.should_raise('[\]')

    # dash
    self.should_raise('[a--z]')
    self.should_raise('[^a--z]')
    self.should_raise('[--z]')
    self.should_raise('[^--z]')
    self.should_raise('[a--]')
    self.should_raise('[^a--]')
    self.should_raise('[---]')
    self.should_raise('[a\-z]')
    raise if RegexDialect.analyze('[-]') != [[[:charclass, false, ['-','-']]]]
    raise if RegexDialect.analyze('[--]') != [[[:charclass, false, ['-','-']]]]
    raise if RegexDialect.analyze('[^-]') != [[[:charclass, true, ['-','-']]]]
    raise if RegexDialect.analyze('[^--]') != [[[:charclass, true, ['-','-']]]]
    raise if RegexDialect.analyze('[-z]') != [[[:charclass, false, ['z','z'], ['-','-']]]]
    raise if RegexDialect.analyze('[a-]') != [[[:charclass, false, ['a','a'], ['-','-']]]]
    raise if RegexDialect.analyze('[-s-]') != [[[:charclass, false, ['s','s'], ['-','-']]]]
    raise if RegexDialect.analyze('[$-]') != [[[:charclass, false, ['$','$'], ['-','-']]]]
    raise if RegexDialect.analyze('[-$]') != [[[:charclass, false, ['$','$'], ['-','-']]]]
    raise if RegexDialect.analyze('[-$-]') != [[[:charclass, false, ['$','$'], ['-','-']]]]
    self.should_raise('[a-b-c]')
    self.should_raise('[^a-b-c]')
    raise if RegexDialect.analyze('[a-z-]') != [[[:charclass, false, ['a','z'], ['-','-']]]]
    raise if RegexDialect.analyze('[-a-z]') != [[[:charclass, false, ['a','z'], ['-','-']]]]
    raise if RegexDialect.analyze('[-a-z-]') != [[[:charclass, false, ['a','z'], ['-','-']]]]

    # range
    self.should_raise('[$-$]')
    self.should_raise('[^$-$]')
    self.should_raise('[0-z]')
    self.should_raise('[a-9]')
    self.should_raise('[Y-b]')
    self.should_raise('[z-y]')
    raise if RegexDialect.analyze('[x-x]') != [[[:charclass, false, ['x','x']]]]
    raise if RegexDialect.analyze('[A-z]') != [[[:charclass, false, ['a','z']]]]
    raise if RegexDialect.analyze('[^i-I]') != [[[:charclass, true, ['i','i']]]]

    # TODO invalid characters
  end

  def self.should_raise(pattern)
    RegexDialect.analyze(pattern)
  rescue
    true
  else
    raise pattern
  end
end








require 'yaml'
require 'json'

RegexDialectTests.run_all_tests

config = YAML.load(ARGF.read)

config["routing"]["rules"].each {|v2ray_rule|
  next unless (domains = v2ray_rule["domain"]) && domains.class == Array
  begin
    domains.map! {|pattern| translate_host_pattern(pattern) }
  rescue => e
    abort e.message
  end
}

puts JSON.pretty_generate(config)
