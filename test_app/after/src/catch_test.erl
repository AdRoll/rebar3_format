-module(catch_test).

can_err(E) ->
    try E of
        {A, B} ->
            ok,
            ok,
            ok,
            ok,
            something_larger_than_ok
    catch
        C:R:Stacktrace ->
            error
    end.

also_can_err() ->
    try
        launch_missiles
    catch
        E:R ->
            error
    end.
