-module(dstree).

-export([new/2, search/1, process/2, add_edge/2, add_edges/2, get_tree/1]).

-export([default_send/3, default_report/1]).

-define(TIMEOUT, 100).

-include_lib("dstree/include/dstree.hrl").
-include_lib("dstree/include/dstree_dev.hrl").

-define(DBG(Format, Args), ok).

%% -define(DBG(Format, Args), ((fun() ->
%%                                      __Now = now(),
%%                                      {_, _, __MS} = __Now,
%%                                      {_, {__Hh, __Mm, __Ss}} = calendar:now_to_local_time(__Now),
%%                                      io:fwrite(user, "[~2..0b:~2..0b:~2..0b.~3..0b] -- " ++ Format ++ "~n", [__Hh, __Mm, __Ss, __MS div 1000] ++ Args)
%%                                      %% io:fwrite(user, "[~2..0b:~2..0b:~2..0b.~3..0b] ~p:~p -- " ++ Format ++ "~n", [__Hh, __Mm, __Ss, __MS div 1000, ?MODULE, ?LINE] ++ Args)
%%                              end)())).

default_send(_Id, X, M) ->
    erlang:send_after(crypto:rand_uniform(1, 100), X, {dstree, M}).

default_report(_State) ->
    ?DBG("~p state: ~p", [self(), _State]).

new(I, Opts) ->
    Neighbors = proplists:get_value(neighbors, Opts, []),
    SendFn = proplists:get_value(send_fn, Opts, fun default_send/3),
    ReportFn = proplists:get_value(report_fn, Opts, fun default_report/1),
    Timeout = proplists:get_value(timeout, Opts, ?TIMEOUT),
    #dstree{status = initial,
            id = I,
            parent = I,
            send_fun = SendFn,
            report_fun = ReportFn,
            waiting_timeout = Timeout,
            neighbors = ordsets:from_list(Neighbors)}.

add_edge(Id, #dstree{id = Id} = State) ->
    State;
add_edge(Node, #dstree{neighbors = N} = State) ->
    State#dstree{neighbors = m(N, [Node])}.

add_edges(N0, #dstree{id = Id, neighbors = Neighbors} = State) when is_list(N0) ->
    N = N0 -- [Id],
    State#dstree{neighbors = m(Neighbors, N)}.

get_tree(#dstree{tree = Tree}) ->
    case Tree of
        undefined -> {error, not_ready};
        _ -> {ok, Tree}
    end.

search(#dstree{id = Id} = State) ->
    process(#forward{origin=Id,sender=Id,visited=[]}, State).

%%
%% initial state
%%
process(#forward{origin= Origin, sender = Sender, visited = Visited},
        #dstree{status = initial,
                id = I,
                neighbors = Neighbors} = State) ->
    ?DBG("~p got forward (~p)", [I, Origin]),
    check(State#dstree{status = waiting,
                       parent = Sender,
                       neighbors = m(Neighbors, Visited),
                       origin = Origin,
                       tree = undefined,
                       subtree = orddict:new(),
                       children = ordsets:new()},
          m(Visited, [I]));

%%
%% waiting state
%%
process(#forward{origin = MOrigin, sender = Sender, visited = Visited},
        #dstree{status = waiting,
                id = I,
                neighbors = Neighbors,
                origin = Origin} = State) ->
    if MOrigin > Origin orelse Origin == undefined ->
            ?DBG("~p got forward (~p)", [I, MOrigin]),
            check(State#dstree{parent = Sender,
                               neighbors = m(Neighbors,Visited),
                               origin = MOrigin,
                               children = ordsets:new()},
                  m(Visited, [I]));
       true ->
            ?DBG("~p ignores ~p (~p =< ~p)", [I, Sender, MOrigin, Origin]),
            State#dstree{neighbors = m(Neighbors,Visited)}
    end;

process(#return{origin = X, sender = Sender, visited = Visited, subtree = ReturnSubtree},
        #dstree{status = waiting,
                children = Children,
                subtree = Subtree,
                origin = Origin,
                id = _I} = State0) when X == Origin ->
    Children2 = m(Children, [Sender]),
    State = maybe_cancel_wait(Sender, State0#dstree{children = Children2}),
    ?DBG("~p got return (~p)", [_I, Origin]),
    Subtree2 = merge_subtree(Children2, Subtree, ReturnSubtree),
    State2 = State#dstree{subtree = Subtree2},
    check(State2, Visited);

process(#working{sender = I} = _Msg,
        #dstree{status = waiting,
                id = I} = State) ->
    State;

process(#working{sender = Child, visited = Visited, waiting_for = Slowpoke} = _Msg, %% one of children seems to be taking long to answer
        #dstree{status = waiting,
                waiting_for = {Child, HalfRef, Ref},
                parent = Parent,
                origin = Origin,
                id = I} = State0) ->
    ?DBG("at ~p some child of ~p is slow ~p ~n", [I, Child, Slowpoke]),
    State = maybe_cancel_wait(Child, State0),
    wait(Child, 2, Visited, State);

process(#timeout{dead = Dead, visited = Visited, attempt = At} = _Msg, %% one of children seems to be taking long to answer
        #dstree{status = waiting,
                waiting_for = {Dead, HalfRef, Ref},
                parent = Parent,
                origin = Origin,
                id = I} = State0) when HalfRef /= undefined ->
    ?DBG("at ~p child is slow ~p (~p)~n", [I, Dead, At]),
    State = State0#dstree{waiting_for = {Dead, undefined, Ref}},
    do_send(#working{origin = Origin,
                     sender = I,
                     waiting_for = Dead,
                     visited = Visited},
            Parent, State);

process(#timeout{dead = Dead, visited = Visited, attempt = At} = _Msg, %% one of children seems to be dead
        #dstree{status = waiting,
                waiting_for = {Dead, undefined, _Ref},
                id = _I} = State0) ->
    ?DBG("at ~p child timeouted  ~p~n", [_I, Dead]),
    State = add_to_dead(Dead, State0#dstree{waiting_for = undefined}),
    check(State, m(Visited, [Dead]));

%%
%% Finishing
%%
process(#finished{origin = X, sender = Sender, tree = Tree} = Msg,
        #dstree{status = finishing,
                id = I,
                origin = Origin} = State0) when X == Origin ->
    ?DBG("at ~p final tree is ~p~n", [I, Tree]),
    State = maybe_cancel_wait(Sender, State0),
    broadcast(Msg#finished{sender = I}, State),
    State2 = State#dstree{status = initial, tree = Tree},
    report(State2);

process(#timeout{dead = Dead, visited = _Visited} = _Msg, %% parent seems to be dead
        #dstree{status = finishing,
                waiting_for = {Dead, _HalfRef, _Ref},
                id = _I} = State) ->
    ?DBG("at ~p parent timeouted ~p~n", [_I, Dead]),
    add_to_dead(Dead, State);

%%
%% Catch all state
%%
process(#timeout{dead = Dead, visited = _Visited} = _Msg, %%GP: sometimes cancel timer can be just slightly too late
        #dstree{id = _I} = State) ->
    ?DBG("at ~p cancel was late ~p~n", [_I, Dead]),
    State;
process(#working{} = _Msg, %%GP: sometimes cancel timer can be just slightly too late
        #dstree{} = State) ->
    State;

process(#return{} = _Msg, #dstree{id = I} = State) -> %%GP: late return may happen
    ?DBG("~p late return ~p~n", [I, _Msg]),
    State;

process(_Msg, #dstree{id = I} = State) ->
    ?DBG("~p unkown message ~p~n", [I, _Msg]),
    erlang:error({unknown_message, _Msg}),
    State.

merge_subtree(Children, Subtree, ReturnSubtree) ->
    L = orddict:to_list(ReturnSubtree),
    %% validate
    [ true = lists:member(K, Children) || {K, _V} <- L ],
    lists:foldl(fun({K,V},S) ->
                        orddict:store(K,V,S)
                end, Subtree, L).

%%
%% Checks if process has visited all it's neighbors.
%%
%%
check(#dstree{id = I,
              parent = Parent,
              neighbors = Neighbors,
              origin = Origin,
              subtree = Subtree} = State,
      Visited) ->
    Unvisited = d(Neighbors, Visited),
    case Unvisited of
        [] ->
            if I == Parent -> %% building a tree is done, we are the root!
                    Tree = {I, Subtree},
                    ?DBG("final spanning tree: ~p~n", [Tree]),
                    State2 = broadcast(#finished{origin = Origin, sender = I, tree = Tree}, State),
                    State3 = State2#dstree{status = initial, tree = Tree},
                    undefined = State3#dstree.waiting_for,
                    report(State3);
               true ->
                    State2 = do_send(#return{origin = Origin,
                                             sender = I,
                                             visited = Visited,
                                             subtree = orddict:from_list([{I, Subtree}])},
                                     Parent, State),
                    wait(Parent, Visited, State2#dstree{status = finishing})
            end;
        [J | _] ->
            ?DBG("~p is not done: ~p - ~p = ~p~n", [I, Neighbors, Visited, Unvisited]),
            State2 = do_send(#forward{origin = Origin, sender = I, visited = Visited},
                             J, State),
            wait(J, Visited, State2#dstree{status = waiting})
    end.

broadcast(M, #dstree{children = Children} = State) ->
    lists:foldl(fun(X, S) ->
                        do_send(M, X, S)
                end, State, dstree_utils:shuffle(ordsets:to_list(Children))).

report(#dstree{report_fun = RF, origin = Origin, tree = Tree, id = I} = State) ->
    RF({done, Origin, Tree, I}),
    State.

do_send(Msg, X, #dstree{id = Id, send_fun = SF} = State) ->
    ?DBG("~p sending ~w to ~p~n", [State#dstree.id, Msg, X]),
    SF(Id, X, Msg),
    State.

m(X, Y) ->
    ordsets:union(X, ordsets:from_list(Y)).
d(X, Y) ->
    ordsets:subtract(X, Y).

wait(Identifier, Visited, State) ->
    wait(Identifier, 1, Visited, State).
wait(Identifier, Multiplier, Visited, #dstree{waiting_timeout = Timeout,
                                              origin = Origin} = State) ->
    Ref = set_timer(Origin, Identifier, 2, Visited, Timeout * Multiplier, State),
    HalfRef = set_timer(Origin, Identifier, 1, Visited, (Timeout * Multiplier) div 2, State),
    State#dstree{waiting_for = {Identifier, HalfRef, Ref}}.

maybe_cancel_wait(Identifier, #dstree{id = Id, waiting_for = {Identifier, HalfRef, Ref}} = State) ->
    ?DBG("at ~p cancelling timeout on ~p~n", [Id, Identifier]),
    cancel_timer(Ref, cancel_timer(HalfRef, State#dstree{waiting_for = undefined}));
maybe_cancel_wait(_Identifier, #dstree{waiting_for = WF} = State) ->
    ?DBG("~p does not match ~p~n", [_Identifier, WF]),
    State.

set_timer(Origin, Identifier, At, Visited, Time, #dstree{id = Id} = State) ->
    Msg = #timeout{origin = Origin, sender = Id, dead = Identifier, visited = Visited, attempt = At},
    {ok, Ref} = dstree_utils:apply_after(Time, erlang, apply,
                                         [fun do_send/3, [Msg, Id, State]]),
    Ref.

cancel_timer(Ref, #dstree{timers = Timers} = State) ->
    ?DBG("Cancelling timer: ~p~n", [Ref]),
    dstree_utils:cancel_timer(Ref),
    State#dstree{timers = orddict:erase(Ref, Timers)}.

add_to_dead(Dead, #dstree{dead = Z} = State) ->
    State#dstree{dead = m(Z, [Dead])}.
