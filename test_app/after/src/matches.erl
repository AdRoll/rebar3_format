-compile(export_all).

-format #{paper => 80, ribbon => 75}.

matches() ->
    % Short
    A = short_stuff,
    A =
        something_very_very_long:and_not_really_divisible_because_its_just_a_function_call(),
    A = something:that(is,
                       totally,
                       divisible and indentable,
                       even,
                       when_it,
                       is,
                       very,
                       long),
    A = [just, a, short, list],
    A = [a,
         much,
         much,
         longer,
         list,
         that,
         probably,
         doesnt,
         fit,
         on,
         a,
         single,
         line],
    A = case this:case_statement() of
            doesnt_fit ->
                in;
            a ->
                line
        end,
    % Exact
    Four = short_stuff,
    Four =
        something_very_very_long:and_not_really_divisible_because_its_just_a_function_call(),
    Four =
        something:that(is,
                       totally,
                       divisible and indentable,
                       even,
                       when_it,
                       is,
                       very,
                       long),
    Four = [just, a, short, list],
    Four =
        [a,
         much,
         much,
         longer,
         list,
         that,
         probably,
         doesnt,
         fit,
         on,
         a,
         single,
         line],
    Four =
        case this:case_statement() of
            doesnt_fit ->
                in;
            a ->
                line
        end,
    % Long
    LongVariableName = short_stuff,
    LongVariableName =
        something_very_very_long:and_not_really_divisible_because_its_just_a_function_call(),
    LongVariableName =
        something:that(is,
                       totally,
                       divisible and indentable,
                       even,
                       when_it,
                       is,
                       very,
                       long),
    LongVariableName = [just, a, short, list],
    LongVariableName =
        [a,
         much,
         much,
         longer,
         list,
         that,
         probably,
         doesnt,
         fit,
         on,
         a,
         single,
         line],
    LongVariableName =
        case this:case_statement() of
            doesnt_fit ->
                in;
            a ->
                line
        end,
    % Mixed
    A = LongVariableName = {That, Has} = More = {than, Four} = characters,
    A = LongVariableName =
            {That, Has} =
                More = {than, Four} = characters:but_must_be_indented(),
    ok.
