-module(funs).

-export [short/0, long/0].

short() ->
    F = fun() -> f end,
    G = fun G() -> G() end,
    H = fun (f) -> F(); (g) -> G() end,
    I = fun I(f) -> H(f); I(g) -> H(g); I(h) -> h end,
    fun(I) -> I(I) end.

long() ->
    FunctionWithLongName = fun() -> {function, with, long, body} end,
    GenericFunctionWithLongName = fun GenericFunctionWithLongName() -> GenericFunctionWithLongName() end,
    HumorouslyLongFunctionName =
        fun (f) -> FunctionWithLongName(); (g) -> GenericFunctionWithLongName()
        end,
    IncrediblyLongFunctionName =
        fun IncrediblyLongFunctionName(f) -> HumorouslyLongFunctionName(f);
            IncrediblyLongFunctionName(g) -> HumorouslyLongFunctionName(g);
            IncrediblyLongFunctionName(h) -> h end,
    fun(IncrediblyLongVariableName) ->
           {IncrediblyLongFunctionName(IncrediblyLongVariableName),
            IncrediblyLongFunctionName(IncrediblyLongVariableName)} end.
