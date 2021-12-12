[V2ray](https://www.v2fly.org/) is a flexible software router and proxy.

Editing v2ray configurations by hand is tedious. This script alleviates the problem by providing a user-friendly alternative config format. Write in this format, then convert to v2ray's format using this script.

`v2ray-config` converts YAML-based config to JSON-based config. But that's not all! `v2ray-config` uses less error-prone defaults and provides further syntactic sugar. It also strips comments.

Usage:

    ./v2ray-config.rb < myconfig.yaml > /etc/v2ray/config.json


The input format is documented in [example input](https://github.com/JackLean/v2ray-config/blob/main/example%20input.yaml).

MIT license
