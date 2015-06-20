provider_asn1
=====

Compile ASN.1 with Rebar3

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { provider_asn1, ".*", {git, "git@github.com:knusbaum/provider_asn1.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 asn
    ===> Fetching provider_asn1
    ===> Compiling provider_asn1
    <Plugin Output>

The command has two options:
 * `--verbose -v` Lots of output.
 * `--clean -c` Remove the generated files
