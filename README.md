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

This will format every Erlang file under `/src` by default. You can specify the directory/file to format as following:

    $ rebar3 format --files src/my_subdir/*.erl
    $ rebar3 format --files src/other_subdir/my_file.erl
    $ rebar3 format --files test/**/*.erl

To save the formatted files in a different directory you have to pass it as a parameter:

    $ rebar3 format --output formatted/

## Configuration

The plugin supports the following configuration options in the `format` section of `rebar.config`:

* `encoding`(`none | epp:source_encoding()`):
    - Encoding to use when writing files.
    - The default value is `none`.
* `paper`(`pos_integer()`):
    - Specifies the preferred maximum number of characters on any line, including indentation.
    - The default value is `100`.
* `ribbon`(`pos_integer()`):
    - Specifies the preferred maximum number of characters on any line, not counting indentation.
    - The default value is `80`.

### Per-File Configuration

You can tweak any of the options above for a particular file, using the `format` attribute in it, like this:

```erlang
-format([{paper, 80}]).
```

## Test

To test the plugin just run `make test`.  
It will copy `test_app` files into `test_app/after` and run `rebar3 format` inside it.  
Add modules with any "tricky" formatting you want to `test_app/src`, and push them to github _including_ the `after` results.  

The `after` results can be tought as the **expected output** behaviour.  
It should be no changes afterwards when running `make test`, unless the format rules got updated.

You can run `make diff` to check diff between `test_app/src` and `test_app/after/src`.
