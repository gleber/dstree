-module(dstree).

-export([new/2, search/1, process/2, add_edge/2, add_edges/2, get_tree/1]).

-export([default_send/2, default_report/1]).

-include_lib("dstree/include/dstree.hrl").
-include_lib("dstree/include/dstree_dev.hrl").

%% -define(DBG(Format, Args), io:fwrite(" src/~p.erl:~p -- " ++ Format ++ "~n", [?MODULE, ?LINE] ++ Args)).
%% -define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format ++ "~n", [?MODULE, ?LINE] ++ Args)).
-define(DBG(Format, Args), ok).

default_send(X, M) ->
    erlang:send_after(crypto:rand_uniform(1, 100), X, {dstree, M}).

default_report(_State) ->
    ?DBG("~p state: ~p", [self(), _State]).

new(I, Opts) ->
    Neighbors = proplists:get_value(neighbors, Opts, []),
    SendFn = proplists:get_value(send_fn, Opts, fun default_send/2),
    ReportFn = proplists:get_value(report_fn, Opts, fun default_report/1),
    Timeout = proplists:get_value(timeout, Opts, 100),
    #dstree{status = initial,
            id = I,
            parent = I,
            send_fun = SendFn,
            report_fun = ReportFn,
            waiting_timeout = Timeout,
            neighbors = ordsets:from_list(Neighbors)}.

add_edge(Node, #dstree{neighbors = N} = State) ->
    State#dstree{neighbors = m(N, [Node])}.

add_edges(N, #dstree{neighbors = Neighbors} = State) when is_list(N) ->
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
    io:format("at ~p some child of ~p is slow ~p ~n", [I, Child, Slowpoke]),
    State = maybe_cancel_wait(Child, State0),
    wait(Child, Visited, State);

process(#timeout{dead = Dead, visited = Visited} = _Msg, %% one of children seems to be taking long to answer
        #dstree{status = waiting,
                waiting_for = {Dead, HalfRef, Ref},
                parent = Parent,
                origin = Origin,
                id = I} = State0) when HalfRef /= undefined ->
    io:format("at ~p child is slow  ~p~n", [I, Dead]),
    State = State0#dstree{waiting_for = {Dead, undefined, Ref}},
    do_send(#working{origin = Origin,
                     sender = I,
                     waiting_for = Dead,
                     visited = Visited},
            Parent, State);

process(#timeout{dead = Dead, visited = Visited} = _Msg, %% one of children seems to be dead
        #dstree{status = waiting,
                waiting_for = {Dead, undefined, _Ref},
                id = _I} = State0) ->
    io:format("at ~p child timeouted  ~p~n", [_I, Dead]),
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
    io:format("at ~p parent timeouted ~p~n", [_I, Dead]),
    add_to_dead(Dead, State);

%%
%% Catch all state
%%
process(#timeout{dead = Dead, visited = _Visited} = _Msg, %%GP: sometimes cancel timer can be just slightly too late
        #dstree{id = _I} = State) ->
    io:format("at ~p cancel was late ~p~n", [_I, Dead]),
    State;
process(#working{} = _Msg, %%GP: sometimes cancel timer can be just slightly too late
        #dstree{} = State) ->
    State;

process(#return{} = _Msg, #dstree{id = I} = State) -> %%GP: late return may happen
    io:format("~p late return ~p~n", [I, _Msg]),
    State;

process(_Msg, #dstree{id = I} = State) ->
    io:format("~p unkown message ~p~n", [I, _Msg]),
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

do_send(Msg, X, #dstree{send_fun = SF} = State) ->
    ?DBG("~p sending ~w to ~p~n", [State#dstree.id, Msg, X]),
    SF(X, Msg),
    State.

m(X, Y) ->
    ordsets:union(X, ordsets:from_list(Y)).
d(X, Y) ->
    ordsets:subtract(X, Y).

wait(Identifier, Visited, #dstree{waiting_timeout = Timeout,
                                  origin = Origin} = State) ->
    Ref = set_timer(Origin, Identifier, Visited, Timeout, State),
    HalfRef = set_timer(Origin, Identifier, Visited, Timeout div 2, State),
    State#dstree{waiting_for = {Identifier, HalfRef, Ref}}.

maybe_cancel_wait(Identifier, #dstree{waiting_for = {Identifier, HalfRef, Ref}} = State) ->
    io:format("Cancelling timeout on ~p~n", [Identifier]),
    cancel_timer(Ref, cancel_timer(HalfRef, State#dstree{waiting_for = undefined}));
maybe_cancel_wait(_Id, #dstree{waiting_for = WF} = State) ->
    io:format("~p does not match ~p~n", [_Id, WF]),
    State.

set_timer(Origin, Identifier, Visited, Time, #dstree{id = Id} = State) ->
    Msg = #timeout{origin = Origin, sender = Id, dead = Identifier, visited = Visited},
    {ok, Ref} = dstree_utils:apply_after(Time, erlang, apply,
                                         [fun do_send/3, [Msg, Id, State]]),
    Ref.

%% add_timer(Origin, Identifier, Time, #dstree{timers = Timers} = State) ->
%%     Msg = #timeout{origin = Origin,
%%                    dead = Identifier},
%%     Ref = set_timer(Origin, Identifier, Time, State),
%%     State#dstree{timers = orddict:store(Ref, Msg, Timers)}.

cancel_timer(Ref, #dstree{timers = Timers} = State) ->
    ?DBG("Cancelling timer: ~p~n", [Ref]),
    dstree_utils:cancel_timer(Ref),
    State#dstree{timers = orddict:erase(Ref, Timers)}.

add_to_dead(Dead, #dstree{dead = Z} = State) ->
    State#dstree{dead = m(Z, [Dead])}.
