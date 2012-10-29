-module(dstree).

-export([new/2, search/1, process/2, add_edge/2, add_edges/2, get_tree/1]).

-export([default_send/2, default_report/1]).

-include_lib("dstree/include/dstree.hrl").
-include_lib("dstree/include/dstree_dev.hrl").

%% -define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format ++ "~n", [?MODULE, ?LINE] ++ Args)).
-define(DBG(Format, Args), ok).

default_send(X, M) ->
    erlang:send_after(crypto:rand_uniform(1, 100), X, {dstree, M}).

default_report(State) ->
    ?DBG("~p state: ~p", [self(), State]).

new(I, Opts) ->
    Neighbors = proplists:get_value(neighbors, Opts, []),
    SendFn = proplists:get_value(send_fn, Opts, fun default_send/2),
    ReportFn = proplists:get_value(report_fn, Opts, fun default_report/1),
    #dstree{status = initial,
            id = I,
            parent = I,
            send_fun = SendFn,
            report_fun = ReportFn,
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
process(_Msg,
        #dstree{status = initial,
                id = I,
                neighbors = Neighbors} = State) ->
    ?DBG("~p unkown message ~p", [I, _Msg]),
    State;

%%
%% waiting state
%%
process(#return{origin = X, visited = Visited, subtree = ReturnSubtree},
        #dstree{status = waiting,
                id = I,
                neighbors = Neighbors,
                children = Children,
                subtree = Subtree,
                origin = Origin} = State) when X == Origin ->
    ?DBG("~p got return (~p)", [I, Origin]),
    Subtree2 = merge_subtree(Children, Subtree, ReturnSubtree),
    State2 = State#dstree{subtree = Subtree2},
    check(State2, Visited);
process(#forward{origin = MOrigin, sender = Sender, visited = Visited},
        #dstree{status = waiting,
                id = I,
                neighbors = Neighbors,
                children = Children,
                subtree = Subtree,
                origin = Origin} = State) ->
    if MOrigin > Origin orelse Origin == undefined ->
            ?DBG("~p got forward (~p)", [I, MOrigin]),
            check(State#dstree{parent = Sender,
                               neighbors = m(Neighbors,Visited),
                               origin = MOrigin,
                               children = ordsets:new()},
                  [I | Visited]);
       true ->
            ?DBG("~p ignores ~p (~p =< ~p)", [I, Sender, MOrigin, Origin]),
            State#dstree{neighbors = m(Neighbors,Visited)}
    end;
process(#finished{origin = X, tree = Tree} = Msg,
        #dstree{status = waiting,
                id = I,
                neighbors = Neighbors,
                children = Children,
                subtree = Subtree,
                origin = Origin} = State) when X == Origin ->
    broadcast(Msg, State),
    State2 = State#dstree{status = initial, tree = Tree},
    report(State2).

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
              children = Children,
              subtree = Subtree} = State,
      Visited) ->
    Unvisited = d(Neighbors, Visited),
    case Unvisited of
        [] ->
            if I == Parent -> %% building a tree is done, we are the root!
                    Tree = {I, Subtree},
                    ?DBG("~nFinal spanning tree: ~p~n~n", [Tree]),
                    State2 = broadcast(#finished{origin = Origin, tree = Tree}, State),
                    State3 = State2#dstree{status = initial, tree = Tree},
                    report(State3);
               true ->
                    State2 = do_send(#return{origin = Origin,
                                             sender = I,
                                             visited = Visited,
                                             subtree = orddict:from_list([{I, Subtree}])
                                            }, Parent, State),
                    State2#dstree{status = waiting}
            end;
        [J | _] ->
            ?DBG("~p is not done: ~p - ~p = ~p~n", [I, Neighbors, Visited, Unvisited]),
            State2 = do_send(#forward{origin = Origin, sender = I, visited = Visited}, J, State),
            State2#dstree{status = waiting,
                          children = m(Children, [J])}
    end.

broadcast(M, #dstree{children = Children} = State) ->
    lists:foldl(fun(X, S) ->
                        do_send(M, X, S)
                end, State, dstree_utils:shuffle(ordsets:to_list(Children))).

report(#dstree{report_fun = RF, origin = Origin, tree = Tree} = State) ->
    RF({done, Origin, Tree}),
    State.

do_send(M, X, #dstree{send_fun = SF} = State) ->
    ?DBG("~p sending ~w to ~p~n", [State#dstree.id, M, X]),
    SF(X, M),
    State.

m(X, Y) ->
    ordsets:union(X, ordsets:from_list(Y)).
d(X, Y) ->
    ordsets:subtract(X, Y).
