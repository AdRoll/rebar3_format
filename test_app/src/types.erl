-module(types).

-type my_int() :: undefined | integer().

-type person() :: #{name := binary() | string(), age := my_int()}.

-type team() :: #{members := [person()], leader := person()}.

-type big_team() :: #{members := [team()], office_address => my_int}.
