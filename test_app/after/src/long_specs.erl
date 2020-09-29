-module(long_specs).

-compile(export_all).

-callback handle_call(term(), {pid(), reference()}, state()) ->
                         {reply, Reply, state()} | {stop, unexpected_call, state()}
     when Reply :: ok | {error, {already_registered, pid()}}.

-spec handle_call(term(), {pid(), reference()}, state()) ->
                     {reply, Reply, state()} | {stop, unexpected_call, state()}
     when Reply :: ok | {error, {already_registered, pid()}}.
handle_call(_, _, _) ->
    ok.

-callback metric(Name, Value, Type) -> Result
     when Name :: binary(), Value :: value(), Type :: metric_type(), Result :: ok.

-spec metric(Name, Value, Type) -> Result
     when Name :: binary(), Value :: value(), Type :: metric_type(), Result :: ok.
metric(_, _, _) ->
    ok.

-callback send(Name :: binary(),
               Value :: binary(),
               Type :: metric_type(),
               Tags :: [bin_kv()]) ->
                  ok.

-spec send(Name :: binary(),
           Value :: binary(),
           Type :: metric_type(),
           Tags :: [bin_kv()]) ->
              ok.
send(_, _, _, _) ->
    ok.

-callback something(With, Multiple, Clauses) -> Result
                       when With :: with,
                            Multiple :: multiple,
                            Clauses :: clauses,
                            Result :: result;
                   (With, Multiple, Clauses) -> Result
                       when With :: [with],
                            Multiple :: [multiple],
                            Clauses :: [clauses],
                            Result :: [result];
                   ({With :: with}, {Multiple :: multiple}, {Clauses :: clauses}) ->
                       {Result :: result}.

-spec something(With, Multiple, Clauses) -> Result
                   when With :: with, Multiple :: multiple, Clauses :: clauses, Result :: result;
               (With, Multiple, Clauses) -> Result
                   when With :: [with],
                        Multiple :: [multiple],
                        Clauses :: [clauses],
                        Result :: [result];
               ({With :: with}, {Multiple :: multiple}, {Clauses :: clauses}) -> {Result :: result}.
something(_, _, _) ->
    result.
