-module(dstree_utils).

-export([shuffle/1, random_pick/2,

         cancel_timer/1, apply_after/4
        ]).

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(crypto:rand_uniform(0, length(List)), List),
    shuffle(Leading ++ T, [H | Acc]).


random_pick(N, List0) ->
    {R,_} = lists:split(N, shuffle(List0)),
    R.

cancel_timer(Ref) ->
    Ref ! cancel.
apply_after(Time, M, F, A) ->
    {ok,
     spawn_link(fun() ->
                        receive
                            cancel -> ok
                        after
                            Time ->
                                apply(M, F, A)
                        end
                end)}.
