# rebar3_format

A rebar plugin for code formatting

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

```erlang
{plugins, [rebar3_format]}
```

Then just call your plugin directly in an existing application:

    $ rebar3 format

This will format every Erlang file under `/src` by default. You can specify the directory/file to format as following:

    $ rebar3 format --files src/my_subdir/*.erl
    $ rebar3 format --files src/other_subdir/my_file.erl
    $ rebar3 format --files test/**/*.erl

To save the formatted files in a different directory you have to pass it as a parameter:

    $ rebar3 format --output formatted/

## Configuration

The plugin supports the following configuration options in the `format` section of `rebar.config`:

* `formatter` (`module()`):
    - This is the module that will dictate the style in which all the code will be formatted. It must implement the `rebar3_formatter` behavior. This project itself provides 2 formatters:
        + `otp_formatter`: Based on the default formatter that comes with Erlang/OTP (`erl_prettypr`), we only fixed some bugs but then respected the original format it produced in its entirety. This formatter only recognizes 2 options:
            * `paper`(`pos_integer()`):
                - Specifies the preferred maximum number of characters on any line, including indentation.
                - The default value is `80`.
            * `ribbon`(`pos_integer()`):
                - Specifies the preferred maximum number of characters on any line, not counting indentation.
                - The default value is `56`.
        + `default_formatter`: Our own default formatter, defining our personal criteria for how to stylize Erlang code. It admits all the options listed below.
    - The default value is `default_formatter`.
* `options` (`#{atom() => term()}`):
    - A map with a list of options that should be interpreted by the chosen `formatter`. The available keys are:
        + `encoding`(`none | epp:source_encoding()`):
            * Encoding to use when writing files.
            * The default value is `none`.
        + `paper`(`pos_integer()`):
            * Specifies the preferred maximum number of characters on any line, including indentation.
            * The default value is `100`.
        + `ribbon`(`pos_integer()`):
            * Specifies the preferred maximum number of characters on any line, not counting indentation.
            * The default value is `90`.
        + `break_indent`(`pos_integer()`):
            * Specifies the preferred number of characters to use to indent a line that "breaks" from the previous one (for instance, a clause body after a clause head).
            * The default value is `4`.
        + `sub_indent`(`pos_integer()`):
            * Specifies the preferred number of characters to use to indent a line that "follows" the current one (for instance, a long clause head or a long function application).
            * The default value is `2`.
        + `inline_items` (`boolean()`):
            * Specifies the desired behavior when using multiple lines for a multi-item structure (i.e. tuple, list, map, etc.).
            * When this flag is on, the formatter will try to fit as many items in each line as permitted by `paper` and `ribbon`.
            * When the flag is off, the formatter will place each item in its own line.
            * The default value is `true`.
        + `inline_expressions` (`boolean()`):
            * Specifies if sequential expressions in a clause should be placed in the same line if `paper` and `ribbon` allows it or if each expression should be placed in its own line.
            * The default value is `true`.
        + `preserve_empty_lines` (`boolean()`):
            * Specifies if blank lines should be preserved when formatting.
            * This option is only used when `inline_expressions` is `false`.
            * If this option is `true`, one empty line will preserved for each group of empty lines that are placed between expressions in a clause.
            * The default value is `false`.
* `files` (`[file:filename_all()]`):
    - List of wildcard patterns representing the files that will be formatted by default (i.e. when not using `--files` on command line).
    - The default value is `["src/**/*.?rl"]`

### Per-File Configuration

You can tweak any of the formatter options for a particular file, using the `format` attribute in it, like this:

```erlang
-format(#{paper => 80}).
```

## Test

To test the plugin just run `rebar3 test`.
It will essentially run `rebar3 format` inside `test_app`.
Add modules with any "tricky" formatting you want to `test_app/src`, and push them to github _including_ the `after` results.
The `after` results can be tought as the **expected output** behaviour.

## Contribute

To contribute to rebar3_format, please refer to [CONTRIBUTING](CONTRIBUTING.md).
