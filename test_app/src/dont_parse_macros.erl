-module(dont_parse_macros).

-format #{parse_macro_definitions => false}.

-define(WITH_A_FLOAT, 1.91).
