-module(dstree).

-export([new/2, search/1, process/2, add_edge/2]).

-export([default_send/2]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("dstree/include/dstree.hrl").

default_send(X, M) ->
    erlang:send_after(crypto:rand_uniform(1, 100), X, {dstree, M}).

random_tree(G, [R|L] = V) ->
    [V1] = dstree_utils:random_pick(1, L),
    digraph:add_edge(G, R, V1),
    random_tree(G, V, 0).

random_tree(G, [Root|Rest] = V, N) ->
    case {lists:all(fun(X) ->
                            digraph:get_path(G, Root, X) /= false
                    end, Rest), N} of
        {true, _} ->
            io:format("READY in ~p attempts~n", [N]),
            ok;
        {_, 10000} ->
            io:format("Not ready in ~p attempts~n", [N]),
            ok;
        {false, _} ->
            [V1, V2] = dstree_utils:random_pick(2, V),
            case {digraph:get_path(G, Root, V1),
                  digraph:get_path(G, Root, V2),
                  digraph:get_path(G, V1, V2)} of
                {false, false, false} -> 
                    digraph:add_edge(G, V1, V2);
                _ ->
                    ok
            end,
            random_tree(G, V, N+1)
    end.

print_graph(G) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = lists:sort(digraph:out_neighbours(G, X)),
          io:format("~p -> ~p~n", [X, N])
      end || X <- V ].

random_test() ->
    DG = digraph:new([acyclic]),
    
    L = [ root | [ list_to_atom("v"++integer_to_list(X)) || X <- lists:seq(1, 15) ] ],

    lists:map(digraph:add_vertex(DG, _), L),
    print_graph(DG),

    random_tree(DG, L),
    print_graph(DG),

    digraph:delete(DG),
    
    %% [ register(X, spawn(dstree, server, [X, dstree_utils:random_pick(L)])) || X <- L ],

    %% [ search(X) || X <- dstree_utils:random_pick(1, L) ],
    %% timer:sleep(1000),
    %% [ X ! stop || X <- L ],
    ok.

basic_test() ->
    L = [v1,v2,v3,v4,v5,v6,v7,v8],
    register(v1,
             spawn(dstree, server, [v1, [v2, v3]])),
    register(v2,
             spawn(dstree, server, [v2, [v3, v5, v4]])),
    register(v3,
             spawn(dstree, server, [v3, [v2, v4, v6]])),
    register(v4,
             spawn(dstree, server, [v4, [v5, v6, v2, v3]])),
    register(v5,
             spawn(dstree, server, [v5, [v2, v4, v6, v7, v8]])),
    register(v6,
             spawn(dstree, server, [v6, [v4, v3, v5, v8]])),
    register(v7,
             spawn(dstree, server, [v7, [v5, v1]])),
    register(v8,
             spawn(dstree, server, [v8, [v5, v6]])),
    search(v1),
    search(v2),
    search(v5),
    search(v8),
    search(v4),
    search(v3),
    timer:sleep(1000),
    [ X ! stop || X <- L ].

new(I, Neighbors) ->
    #dstree{status = initial,
            id = I,
            parent = I,
            neighbors = orddict:from_list(Neighbors)}.

search(#dstree{id = Id} = State) ->
    process({dstree, {forward, Id, [], Id}}, State).

process(Msg, #dstree{status = initial,
                     id = I,
                     parent = Parent,
                     neighbors = Neighbors} = State) ->
    case Msg of
        {forward, Sender, Visited, Origin} ->
            io:fwrite("~s got forward (~s)\n", [I, Origin]),
            check(State#dstree{status = process,
                               parent = Sender,
                               neighbors = m(Neighbors, Visited),
                               origin = Origin,
                               children = orddict:new()},
                  [I | Visited]);
        report ->
            io:fwrite("~s report: parent - ~s | ~p\n", [I, Parent, orddict:to_list(Neighbors)]),
            broadcast(report, State);
        _Msg ->
            io:fwrite("~s unkown message ~p~n", [I, _Msg]),
            State
    end;

process(Msg, #dstree{status = waiting,
                     id = I,
                     parent = Parent,
                     neighbors = Neighbors,
                     origin = Origin} = State) ->
    case Msg of
        report ->
            io:fwrite("~s report: parent - ~s | ~p\n", [I, Parent, orddict:to_list(Neighbors)]),
            broadcast(report, State);
        {return, Visited, X} when X == Origin ->
            io:fwrite("~s got return (~s)\n", [I, Origin]),
            check(State, Visited);
        {forward, Sender, Visited, MOrigin} ->
            if MOrigin > Origin ->
                    io:fwrite("~s got forward (~s)\n", [I, MOrigin]),
                    check(State#dstree{parent = Sender,
                                       neighbors = m(Neighbors,Visited),
                                       origin = MOrigin,
                                       children = orddict:new()},
                          [I | Visited]);
               true ->
                    io:fwrite("~s ignores ~s (~s)\n", [I, Sender, Origin]),
                    State#dstree{neighbors = m(Neighbors,Visited)}
            end;
        {finished, X} when X == Origin ->
            broadcast({finished, Origin}, State),
            State#dstree{status = initial}
    end.

check(#dstree{id = I,
              parent = Parent,
              neighbors = Neighbors,
              origin = Origin,
              children = Children} = State,
      Visited) ->
    Unvisited = Neighbors -- Visited,
    case Unvisited of
        [] ->
            if I == Parent ->
                    State2 = broadcast({finished, Origin}, State),
                    State3 = do_send(report, I, State2),
                    State3#dstree{status = initial};
               true ->
                    State2 = do_send({return, Visited, Origin}, Parent, State),
                    State2#dstree{status = waiting}
            end;
        [J | _] ->
            State2 = do_send({forward, I, Visited, Origin}, J, State),
            State2#dstree{status = waiting,
                          children = m(Children, [J])}
    end.


broadcast(M, #dstree{children = Children} = State) ->
    lists:foldl(fun(X, S) ->
                        do_send(M, X, S)
                end, State, dstree_utils:shuffle(orddict:to_list(Children))).

do_send(M, X, #dstree{send_fun = SF} = State) ->
    SF(X, M),
    State.

add_edge(Node, #dstree{neighbors = N} = State) ->
    State#dstree{neighbors = m(N, [Node])}.

m(X, Y) ->
    orddict:merge(X, orddict:from_list(Y)).
