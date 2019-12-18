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

* `files` (`[file:filename_all()]`):
    - List of wildcard patterns representing the files that will be formatted by default (i.e. when not using `--files` on command line).
    - The default value is `["src/**/*.?rl"]`
* `encoding`(`none | epp:source_encoding()`):
    - Encoding to use when writing files.
    - The default value is `none`.
* `paper`(`pos_integer()`):
    - Specifies the preferred maximum number of characters on any line, including indentation.
    - The default value is `100`.
* `ribbon`(`pos_integer()`):
    - Specifies the preferred maximum number of characters on any line, not counting indentation.
    - The default value is `80`.
* `break_indent`(`pos_integer()`):
    - Specifies the preferred number of characters to use to indent a line that "breaks" from the previous one (for instance, a clause body after a clause head).
    - The default value is `4`.
* `sub_indent`(`pos_integer()`):
    - Specifies the preferred number of characters to use to indent a line that "follows" the current one (for instance, a long clause head or a long function application).
    - The default value is `2`.
* `remove_tabs` (`boolean()`):
    - Erlang's `prettypr` inserts a tab character each time it has to insert 8 spaces for indentation and that code is in a 100% unconfigurable/unreplaceable/unhookable function. If this setting is `true`, the formatter will turn those tabs into 8 spaces again.
    - The default value is `true`.
    - **NOTE:** We are aware that `true` is not the _actual OTP default_ but... **really?** Who wants their code indented with a mixture of tabs and spaces? 🙈
* `remove_trailing_spaces` (`boolean()`):
    - If this setting is `true`, the formatter will remove all trailing whitespaces.
    - The default value is `true`.
* `inline_expressions` (`boolean()`):
    - Specifies if sequential expressions in a clause should be placed in the same line if `paper` and `ribbon` allows it or if each expression should be placed in its own line.
    - The default value is `true`.
* `preserve_empty_lines` (`boolean()`):
    - Specifies if blank lines should be preserved when formatting.
    - This option is only used when `inline_expressions` is `false`.
    - If this option is `true`, one empty line will preserved for each group of empty lines that are placed between expressions in a clause.
    - The default value is `false`.

### Per-File Configuration

You can tweak any of the options above for a particular file, using the `format` attribute in it, like this:

```erlang
-format([{paper, 80}]).
```

## Test

To test the plugin just run `rebar3 test`.
It will essentially run `rebar3 format` inside `test_app`.
Add modules with any "tricky" formatting you want to `test_app/src`, and push them to github _including_ the `after` results.
The `after` results can be tought as the **expected output** behaviour.

## Contribute

To contribute to rebar3_format, please refer to [CONTRIBUTING](CONTRIBUTING.md).
