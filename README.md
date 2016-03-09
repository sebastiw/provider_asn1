provider_asn1
=====

Compile ASN.1 with Rebar3

Build
-----

    $ rebar3 asn compile

Use
---

The plugin can be accessed via hex.pm:
```
{plugins, [
    rebar3_hex,
    { provider_asn1, "0.2.0"}
]}.
```

You can also pull the plugin directly from git:
```
{plugins, [
    { provider_asn1, ".*", {git, "git@github.com:knusbaum/provider_asn1.git", {tag, "0.2.0"}}}
]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 asn compile
===> Fetching provider_asn1
===> Compiling provider_asn1
<Plugin Output>
```

The plugin will look in an app directory called 'asn1' for *.asn1 files, compile them, and move the generated source to the appropriate places.

For example, Your project should have a structure like this:
```
.
├── LICENSE
├── README.md
├── asn1
│   └── Files.asn1
├── include
├── rebar.config
├── rebar.lock
└── src
    └── some_app.erl
```
Then if you run `rebar3 asn`, your asn.1 files will be compiled:
```
$ rebar3 asn compile
===> Fetching provider_asn1 ({git,
                                     "git@github.com:knusbaum/provider_asn1.git",
                                     {tag,"0.2.0"}})
===> Compiling provider_asn1
===> Generating ASN.1 files.
$ tree
.
├── LICENSE
├── README.md
├── _build
│   └── ...
├── asn1
│   └── Files.asn1
├── asngen
│   ├── Files.asn1db
│   ├── Files.erl
│   └── Files.hrl
├── include
│   └── Files.hrl
├── rebar.config
├── rebar.lock
└── src
    ├── Files.asn1db
    ├── Files.erl
    └── some_app.erl
```

The provider also has a `clean` command which will remove generated files.
```
$ rebar3 asn clean
===> Cleaning ASN.1 generated files.
```

You may also want to add a hook to the 'compile' and 'clean' phases, so you don't have to manually generate and remove the asn files

rebar.config:
```
...
{provider_hooks, [{pre, [{compile, {asn, compile}}]},
                  {post, [{clean, {asn, clean}}]}]}.
...
```
Now your asn.1 files will be compiled before your other files, so they'll always be available when you build.

The command has two options:
 * `--verbose -v` Lots of output.
