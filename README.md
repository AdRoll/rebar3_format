# rebar3_format

A rebar plugin for code formatting

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

```erlang
{profiles, [rebar3_format]}
```

Then just call your plugin directly in an existing application:

    $ rebar3 format

## Configuration

The plugin supports the following configuration options in the `format` section of `rebar.config`:

* `includes`(`[file:name()]`):
    - List of paths where to find hrl files
* `macros`(`epp:macros()`):
    - List of predefined macros
* `encoding`(`none | epp:source_encoding()`):
    - Encoding to use when writing files
* `paper`(`pos_integer()`):
    - Specifies the preferred maximum number of characters on any line, including indentation. The default value is `100`.
* `ribbon`(`pos_integer(`):
    - Specifies the preferred maximum number of characters on any line, not counting indentation. The default value is `80`.

## Test

To test the plugin, get into the `test_app` folder and run `rebar3 format`.
Add modules with any "tricky" formatting you want to it, but push them to github _without_ formatting them first. If possible add examples of the expected results, too.
