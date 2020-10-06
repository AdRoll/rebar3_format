# rebar3_format

[![Build Status](https://api.travis-ci.com/AdRoll/rebar3_format.svg?branch=master)](https://travis-ci.com/AdRoll/rebar3_format)
[![Hex pm](http://img.shields.io/hexpm/v/rebar3_format.svg?style=flat)](https://hex.pm/packages/rebar3_format)

A rebar plugin for code formatting

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

```erlang
{project_plugins, [rebar3_format]}
```

Then just call your plugin directly in an existing application:

    $ rebar3 format

This will format every Erlang file under `/src` by default. You can specify the directory/file to format as following:

    $ rebar3 format --files 'src/my_subdir/*.erl'
    $ rebar3 format --files src/other_subdir/my_file.erl
    $ rebar3 format --files 'test/**/*.erl' --files 'include/*.hrl'

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
        + `unquote_atoms` (`boolean()`):
            * Specifies whether the formatter should remove quotes from atoms that don't need them (e.g. `'this_one'`) or not.
            * The default value is `true`, i.e. the formatter won't preserve your quotes if they're not needed, unless you explicitely ask for.
        + `inline_attributes` (`all | none | {when_over, pos_integer()}`):
            * Specifies the desired behavior for inlining attributes with lists, like `-export`, `-export_type` and `-optional_callbacks`.
            * When this option is `all`, the formatter will try to fit as many items in each line as permitted by `paper` and `ribbon`.
            * When the flag is `none`, the formatter will place each item in its own line.
            * When the flag is `{when_over, N}` the formatter will work as `none` for lists with up to `N` elements, and it will inline longer lists.
            * The default value is `all`, i.e. always put as many functions/types on each row as possible.
        + `inline_items` (`all | none | {when_over, pos_integer()}`):
            * Specifies the desired behavior when the formatter needs to use multiple lines for a multi-item structure (i.e. tuple, list, map, etc.).
            * **NOTE:** If the formatter can put all items in the same row, it will do it, regardless of this configuration.
            * When this option is `all`, the formatter will try to fit as many items in each line as permitted by `paper` and `ribbon`.
            * When the flag is `none`, the formatter will place each item in its own line.
            * When the flag is `{when_over, N}` the formatter will work as `none` for lists with up to `N` elements, and it will inline longer lists.
            * The default value is `{when_over, 25}` to properly accommodate large binaries or lists.
        + `inline_simple_funs` (`boolean()`):
            * Specifies if anonymous function bodies should be placed in the same line as the function clause head in case for anonymous functions with just one clause if `paper` and `ribbon` allows it or if these simple funs should be indented as all the others.
            * The default value is `true`.
        + `inline_qualified_function_composition` (`boolean()`):
            * Specifies if composed qualified function calls (e.g. `module1:function1(module2:function2(...`) should stay in the same line if they fit or if the formatter should always put the internal function call in the next line.
            * Because of how OTP's `prettypr` is built (which is the tool we're using to finally print the formatted code) we can't indent these function calls _only if it doesn't fit in a line_, at least not without adding an extra space to the right of `(` for _all_ function applications. That's why this switch is all-or-none.
            * The non-inlining only applies when both functions that are composed are fully qualified (we're using _fully-qualified_ as a _proxy_ for _has a long name_); e.g. in this case `d(f:f(g:g(h(…))))` the formatter will always write `g:g(...)` in the next row, but not `h(...)` nor `f:f(...)` will be moved to a new row.
            * The default value is `false`.
        + `inline_clause_bodies` (`boolean()`):
            * Specifies if clause bodies (for `case`, `function`, etc. statements) should be placed in the same line as the clause heads if `paper` and `ribbon` allows it or if all bodies should be placed in the next line after their clause heads.
            * The default value is `false`.
        + `inline_expressions` (`boolean()`):
            * Specifies if sequential expressions in a clause should be placed in the same line if `paper` and `ribbon` allows it or if each expression should be placed in its own line.
            * The default value is `false`.
        + `parenthesize_infix_operations` (`boolean()`):
            * Specifies if parentheses should be added around composed [infix operations](https://erlang.org/doc/reference_manual/expressions.html#arithmetic-expressions) to avoid confusion around precedence.
            * The default value is `false`.
        + `preserve_empty_lines` (`boolean()`):
            * Specifies if blank lines between statements should be preserved when formatting.
            * Keep in mind that blank lines between clauses, between items in tuples, lists, etc, between attributes, and so on will not be affected by this configuration and therefore they'll be unconditionally removed.
            * This option is only used when `inline_expressions` is `false`.
            * If this option is `true`, one empty line will preserved for each group of empty lines that are placed between expressions in a clause.
            * The default value is `true`.
* `files` (`[file:filename_all()]`):
    - List of wildcard patterns representing the files that will be formatted by default (i.e. when not using `--files` on command line).
    - The default value is `["src/**/*.?rl"]`
* `ignore` (`[file:filename_all()]`):
    - List of wildcard patterns representing the files that the formatter will ignore when formatting.
    - Note that it will ignore the files set for formatting either with the `files` option or using `--files` in the command line if they match one of the given wildcards.
    - You can also ignore a specific file adding the attribute `-format(ignore)` in it.

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

---

## Proposed Workflow
When we created this tool, we envisioned a workflow for teams where each member can use their preferred style for code formatting.
The idea is to take advantage of `rebar3` profiles and write the following on your `rebar.config` file:

```erlang
%% The canonical format used when pushing code to the central repository
{format, [
    {files, ["src/*.erl", "include/*.hrl", "test/*.erl"]},
    {formatter, default_formatter},
    {options, #{paper => 100}}
]}.
{profiles, [
    {brujo, [
        {format, [
            {files, ["src/*.erl", "include/*.hrl", "test/*.erl"]},
            {formatter, rok_formatter}, % I prefer comma-first formatting
            {options, #{paper => 100}}
        ]}
    ]},
    {miriam, [
        {format, [
            {files, ["src/*.erl", "include/*.hrl", "test/*.erl"]},
            {formatter, default_formatter},
            {options, #{
                inline_clause_bodies => false, % she doesn't like one-liners
                inline_simple_funs => false, % and she's adamant about it
                inline_items => all % but she does like long lists of items
            }}
        ]}
    ]}
]}
```

Then whenever you're about to work on something, follow this ritual:

```bash
git checkout master
git checkout -b my-branch
rebar3 as brujo format
# Work on your code...
rebar3 format # This can be a git hook for commits
git commit -am "Apply my changes"
git push origin my-branch --set-upstream
```

Other developers do the same but using `as $THEIR_NAME` instead of `as brujo`.

That way each developer can read code in the way they understand it better, write code exactly how they like to write it, etc. Then push it to the central repository in a consistent way that matches the style of the rest of the project.

---

## Using External Formatters
Through `rebar3 format`, you can use other formatters that are not included in this repository. That way you can follow our proposed workflow and allow each developer to format the code with their favorite formatter using rebar3 plugins while still maintaining an unique _canonical formatter_ when pushing to your central git repository.
You also get `-format` attribute compliance (including `-format ignore.`) for free, since they're respected when using any formatter.

### Steamroller
If you want to use @old-reliable's [steamroller](https://github.com/old-reliable/steamroller), you just need to add the following things to your `rebar.config` file:

```erlang
{project_plugins, [rebar3_format, steamroller]}.

{format, [
{files, ["src/*.erl", "include/*.hrl"]},
{ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl"]},
{formatter, sr_formatter}, %% The steamroller formatter.
{options, #{line_length => 80}}
]}.
```

### erlfmt
If you want to use [@whatsapp](https://github.com/whatsapp)'s [erlfmt](https://github.com/whatsapp/erlfmt), you just need to add the following things to your `rebar.config` file:

```erlang
{project_plugins, [rebar3_format, erlfmt]}.
{format, [
    {files, ["src/*.erl", "include/*.hrl"]},
    {ignore, ["src/*_ignore.erl", "src/ignored_file_config.erl"]},
    {formatter, erlfmt_formatter}, %% The erlfmt formatter interface.
    {options, #{print_width => 100, ignore_pragma => true}} %% ...or no options at all.
]}.
```

#### Compatibility Note
`erlfmt_formatter` is compatible with version `v0.7.0` and `v0.8.0` of `erlfmt`, which are currently available at [hex.pm](https://hex.pm/packages/erlfmt).


## Implementing your own Formatter

To create a new formatter, you need to implement the `rebar3_formatter` behaviour. It defines just one callback:

```erlang
-callback format(file:filename_all(), opts()) -> result().
```

That means you need to write a function that receives a filename and a map with options (some of them are specified in the `rebar3_formatter` module, but you can add as many others as you want) and returns a result (either `changed` or `unchanged`). It's expected for your formatter to honor the predefined options as described below:

* `output_dir`:
    - `none`: Don't produce any output.
    - `current`: Replace files when formatting.
    - `file:filename_all()`: Drop files in this folder, preserving their current names.
* `encoding`:
    - `none`: Preserve/guess original encoding of files.
    - `epp:source_encoding()`: Use this encoding when parsing files.
* `action`:
    - `verify`: Only return the result without actually modifying any files.
    - `format`: Do format the files.

It's a good practice, although not enforced by the formatter itself to respect `-format` attributes in files as the formatters provided in this repo do.
To remove the need for parsing and writing files, you can use the `rebar3_ast_formatter` module/behaviour as `default_formatter` and `otp_formatter` do.

## Editor Integration

### Visual Studio Code

You can use rebar3_format from Visual Studio Code with the
[Erlang Formatter](https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter)
extension.

## Helpers

### Git commit hooks

In the `scripts` folder you'll find two scripts that work really well as _pre_ and _post_ commit git hooks, in case you want to _slowly_ format your huge repos with a myriad of modules :)

## Contribute

To contribute to rebar3_format, please refer to [CONTRIBUTING](CONTRIBUTING.md).
