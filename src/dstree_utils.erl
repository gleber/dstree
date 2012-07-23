-module(dstree_utils).

-export([shuffle/1, random_pick/2]).

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).


random_pick(N, List0) ->
    {R,_} = lists:split(N, shuffle(List0)),
    R.
