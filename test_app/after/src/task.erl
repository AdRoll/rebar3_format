-module(task).

-vsn(1.00000000000000000000e+00).

-opaque task_spec() :: {Name :: atom(), mfa()}.

-type reason() :: string() | binary().

-export_type([task_spec/0]).

-export([name/1, mfa/1]).

%% @doc Stops the given task.
-callback stop(Name :: atom()) -> ok | {error, reason()}.

%% @doc Starts the specified task.
-callback start(task_spec()) -> any().

-spec name(Spec :: task_spec()) -> atom().

name({Name, _MFA}) -> Name.

-spec mfa(task_spec()) -> mfa().

mfa({_, MFA}) -> MFA.

