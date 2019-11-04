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

The plugin supports the following configuration options in `rebar.config`:

**TODO:** Add Configuration

## Test

To test the plugin, get into the `test_app` folder and run `rebar3 format`.
Add modules with any "tricky" formatting you want to it, but push them to github _without_ formatting them first. If possible add examples of the expected results, too.
