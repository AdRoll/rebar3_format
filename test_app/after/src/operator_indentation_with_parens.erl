-module(operator_indentation_with_parens).

-format #{parenthesize_infix_operations => true}.

a_large_predicate(PredOne, PredTwo, AValue) ->
    (something:very_long(based,
                         on,
                         PredOne,
                         "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
     andalso (PredTwo
              andalso ((AValue =:= something)
                       andalso something:potentially(
                                   very:long(
                                       that:needs(indenting))))))
    orelse ((((PredOne and PredTwo) and something:potentially(very, extremely, long))
             and (AValue =:= something_else))
            orelse (PredOne
                    andalso (something:very_long(based,
                                                 on,
                                                 PredOne,
                                                 "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
                             andalso ((AValue =:= something_other)
                                      andalso something:potentially(
                                                  very:long(
                                                      that:needs(indenting))))))).

in_a_list(PredOne, PredTwo, AValue) ->
    [something:very_long(based,
                         on,
                         PredOne,
                         "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
     andalso (PredTwo
              andalso ((AValue =:= something)
                       andalso something:potentially(
                                   very:long(
                                       that:needs(indenting))))),
     ((PredOne and PredTwo) and something:potentially(very, extremely, long))
     and (AValue =:= something_else),
     PredOne
     andalso (something:very_long(based,
                                  on,
                                  PredOne,
                                  "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
              andalso ((AValue =:= something_other)
                       andalso something:potentially(
                                   very:long(
                                       that:needs(indenting)))))].

precedence(One, Two, Three) ->
    careful:with(the,
                 precedence,
                 ((One -- Two) -- Three) -- One,
                 is_not,
                 One -- ((Two -- Three) -- One),
                 nor,
                 One -- (Two -- (Three -- One))).

other_operators(PredOne, PredTwo, AValue) ->
    something:very_long(based,
                        on,
                        PredOne,
                        "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
    ++ (PredTwo
        ++ ((AValue xor something)
            ++ (something:potentially(
                    very:long(
                        that:needs(indenting)))
                -- ((((PredOne + PredTwo) + something:potentially(very, extremely, long))
                     + (AValue band something_else))
                    -- (PredOne
                        ++ (something:very_long(based,
                                                on,
                                                PredOne,
                                                "1231231231231231313123123132sfsafsasfasdfasdfasdfsdf")
                            ++ ((AValue band something_other)
                                ++ something:potentially(
                                       very:long(
                                           that:needs(indenting)))))))))).
