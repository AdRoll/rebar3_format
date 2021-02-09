-module(should_be_in_parentheses).

-export([should/1, have_parentheses/0]).

-hank ignore.

-elvis ignore.

-format #{should_not => have_parentheses}.

-type should() :: {'not', have, parentheses}.

-this({attribute, should, have, parentheses}).

-this_one_the_formater_should ignore.

-spec should(Not) -> have:parentheses().
should(Not) ->
    have:parentheses().
