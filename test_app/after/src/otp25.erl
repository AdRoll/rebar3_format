%% @doc New stuff introduced in OTP25.
-module(otp25).

-feature(maybe_expr, enable).

-export(['maybe'/0, 'else'/0, short/0]).

'maybe'() ->
    maybe
        'maybe' ?= multiple:expressions(),
        with ?= question:equal(),
        which ?=
            should_indent_properly_even_if:the_next_thing(is_very_very_extremely_long_and_goes_beyond_the_margin),
        which ?=
            should_indent_properly_even_if:the_next_thing(is_very_very_extremely_long,
                                                          and_goes_beyond_the_margin)
    end.

'else'() ->
    OneLiner =
        maybe
            ok ?= ok
        else
            _ ->
                ng
        end,
    MultiLiner =
        maybe
            ok ?= ok
        else
            _ ->
                more:than(),
                one:expression(),
                in:the_else()
        end,
    {OneLiner, MultiLiner}.

short() ->
    maybe
        A ?= one:liner()
    end,
    A.
