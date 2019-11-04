-module(task).

-vsn(1.0).

-opaque task_spec() :: {Name :: atom(), mfa()}.

-type reason() :: string() | binary().

-export_type([task_spec/0]).

-export([name/1, mfa/1]).

-callback stop(Name :: atom()) -> ok | {error, reason()}.

-callback start(task_spec()) -> any().

-spec name(Spec :: task_spec()) -> atom().

name({Name, _MFA}) -> Name.

-spec mfa(task_spec()) -> mfa().

mfa({_, MFA}) -> MFA.

