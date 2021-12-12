# About this document

We specify a regular expression dialect below.  For convenience, we will refer to it informally as AltRegex.

AltRegex is almost a subset of Go regex.  Most features are either removed or identical to Go
regex, with very few additions/modifications.  [Quick introduction](#Quick-introduction) describes everything you need to
know to start using AltRegex.

More precisely, we classify regex elements in 4 categories:
  - valid in Go regex but invalid in AltRegex (removal)
  - valid in AltRegex but invalid in Go regex (addition)
  - valid in AltRegex and Go regex, and behaves identically (common factor)
  - valid in AltRegex and Go regex, but behaves differently (potential surprise)

"Quick introduction" describes addition and potential surprise.  That's all you need to know,
because the script will halt if you use a removed feature, and you will never be surprised by a
silent error.  The [full specification](#Full-specification) describes all 4 categories.


# Quick introduction

AltRegex is designed with hostname matching in mind.  As a result, its syntax looks very different
from a conventional regex.  But its semantics closely mirror Go's semantics.

Go regex is enclosed in single slash    `/^[a-z.]*$/`<br>
AltRegex is enclosed in double slash    `//[a-z.]*//`

### Rule 1
An AltRegex always matches the entire hostname, so you don't have to write `^` and `$`.  In fact, `^`
is usually invalid in AltRegex (and `$` is literal).

### Rule 2
`.` is a literal.<br>
`:` matches any character except a dot.<br>
`,` matches any character.

```
    //.//     is equivalent to     /^\.$/
    //,//     is equivalent to     /^.$/
    //://     is equivalent to     /^[^.]$/
```

### Rule 3
Space, tabs, and newlines are allowed in most positions.  They are ignored.  They can be used for
readability.

Uppercase letters are equivalent to lowercase letters.

```
    //  en  .  wi ki pedia  .  org  //    is equivalent to     /^en\.wikipedia\.org$/
    //[a - G] [H-n] .WikiPedia.ORG//      is equivalent to     /^[a-g][h-n]\.wikipedia\.org$/
```

### Rule 4
There is no distinction between greedy and lazy repetition.
```
    //a*//   is equivalent to   /^a*$/         which is equivalent to    /^a*?$/
    //a+?//  is equivalent to   /^(a+)?$/      NOT to    /^a+?$/
```

In addition to `{n}`, `{n,}`, `{n,m}`, AltRegex supports `{,m}` repetitions.  Leading zeroes are allowed.

### Rule 5
The following special characters can be escaped:
```
   , ( ) * +
```
The following characters are literal characters (not special):
```
   . $
```
The following special characters can't be escaped because you will never need them:
```
   \ : ? [ ] { } | ^
```

Escapes are not allowed in `[abc]`.  You don't need them.



# Full specification

an *AltRegex pattern* = 1 or more *lists of atoms* seperated by `|`

a *list of atom* = 0 or more *atoms*

The pattern matches if one of the *lists* matches.  A *list of atom* matches if the text can be split
into several parts, each matching a corresponding *atom*.

an *atom* = one of
  - SPACE TAB LF = ignored
  - `,` = matches any byte
  - `:` = matches any byte except `.`
  - a literal punctuation = matches itself    (a literal punctuation is one of `. $ - _ ! " % & ' ; = ~`)
  - a digit = matches itself
  - a lowercase letter = matches itself
  - an uppercase letter = matches the corresponding lowercase letter
  - an *escape* (see below)
  - `[` ... `]` = a *character class* (see below)
  - an *atom* followed by a *repetition modifier* = a *repetition*
  - `(` ... `)` = treat the *pattern* inside as a whole

Anything else is invalid.

a *repetition* = one of
  - *atom-ws* `*` = *atom* repeated 0 or more times
  - *atom-ws* `+` = *atom* repeated 1 or more times
  - *atom-ws* `?` = *atom* repeated 0 or 1 times
  - *atom-ws* `{` *n* `}` = *atom* repeated *n* times
  - *atom-ws* `{` *n* `,}` = *atom* repeated at least *n* times
  - *atom-ws* `{,` *m* `}` = *atom* repeated at most *m* times
  - *atom-ws* `{` *n* `,` *m* `}` = *atom* repeated at least *n* times and at most *m* times (*n* ≤ *m*)

*n* and *m* are strings of 1 or more digits. Leading zeroes are allowed.

*atom-ws* = an *atom* followed by 0 or more SPACE/TAB/LF<br>
Whitespace is disallowed inside `{}`

The inner *atom* is repeated and then matched, not matched first and then repeated.<br>
For example, `//(a|b){2}//` matches `ab` `ba` in addition to `aa` and `bb`.
To match the same string repeated twice, you must use `//a{2}|b{2}//`.

an *escape* = one of
  - `\,` = matches `,`
  - `\*` = matches `*`
  - `\+` = matches `+`
  - `\(` = matches `(`
  - `\)` = matches `)`
  - `\d` = matches a digit
  - `\D` = matches a non-digit character (equivalent to `[^0-9]`)
  - `\w` = matches a "word character" (equivalent to `[0-9A-Za-z_]`, which is equivalent to `[0-9a-z_]`)
  - `\W` = matches a non-word character (equivalent to `[^0-9a-z_]`)
  - `\b` = matches an empty string at word boundary (per Go regex)
  - `\B` = matches an empty string at a non-word-boundary

a *character class* = `[` *nws*  (optional `^`)  *nws*  (1 or more *class-elems*, seperated by *nws*)  *nws* `]`

*class-elem* = one of
  - `-` = matches itself
  - a *class-char* = matches itself
  - *class-char* *nws* `-` *nws* *class-char* = a *range*

*nws* = 0 or more SPACE/TAB/LF

*class-char* =
  - one of `. , * + $ ( ) ! " % & ' ; = ~ _`
  - or a letter (an uppercase letter is treated as if it's its lowercase counterpart)
  - or a digit

A class matches if any of its elements matches.  If `^` is present, it matches if all of its
elements unmatch instead.

Duplicated/overlapping elements allowed.  If `-` is an element, it must be the first or the last.

Both sides of a *range* must be digits, or both of them must be letters.  For a letter range,
uppercase letters are converted to their lowercase counterparts first.  The left side must be
≤ the right side.

A *range* matches character *x* if left ≤ *x* ≤ right.



# Clarifications
The following are not part of the specification.  They clarify the specification to remove confusion.

Non-ASCII characters are disallowed. Control characters (except tabs and newlines) are disallowed.
Specifically, \r is disallowed.
To specify an [internationalized](https://en.wikipedia.org/wiki/Internationalized_domain_name#ToASCII_and_ToUnicode) hostname,
use the actual [XN-label](https://datatracker.ietf.org/doc/html/rfc5890#section-2.3.1).

``#/@`<>`` are disallowed anywhere.  `^` is disallowed except in character classes.  `:?|{}[\` are
disallowed inside character classes.

Space/tab/newline are disallowed inside `{}` or inside an escape sequence.

Embedded comments are not supported.

[URI standard](https://datatracker.ietf.org/doc/html/rfc3986) allow uppercase letters and define hostnames as case-insensitive strings,
but v2ray always [convert hostnames](https://github.com/v2fly/v2ray-core/blob/v4.41.0/app/router/condition.go#L105) to lowercase strings.
That's why AltRegex never considers uppercase letters.

Unicode character classes (like `\pL \p{L}`), ASCII character classes (like `[:alnum:]`) and `\s \S`
are not allowed anywhere.  `\d \D \w \W` are disallowed inside character classes.  Nested character classes are disallowed.

Back references are not supported.  Non-capturing groups, named captures, and group flags are
not supported.

`\A \z \a \f \t \n \r \v` are not allowed.

An already literal character must not be escaped.

If there are two `-` elements in a character class, one must be the first, one must be
the last: `//[--]//`.

Lazy repetitions are not supported. See [quick introduction](#Quick-introduction).

Go regex does not allow n > 1000 or m > 1000 in repetitions.  AltRegex allows them, but the
resulting regex will be rejected by v2ray.

`{,m}` is an AltRegex addition.  It is not available in Go.
