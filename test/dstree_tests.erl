-module(dstree_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format, [?MODULE, ?LINE] ++ Args)).
-define(QC(Arg), proper:quickcheck(Arg, [{on_output, printer()}])).

-export([printer/0, report_fun/1, connect/2, print_graph/1]).

printer() ->
    fun(F, A) ->
            io:fwrite(user, F, A)
    end.

proper_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [
      {timeout, 100, fun dstree_prop:test_prop_simple/0},
      {timeout, 100, fun dstree_prop:test_prop_all/0},
      fun() -> ok end]}.
    
random_tree(G, [R|L]) ->
    random_tree(G, [R], dstree_utils:shuffle(L)).

random_tree(G, _Visited, []) ->
    G;

random_tree(G, Visited, [V2|R]) ->
    [V1] = dstree_utils:random_pick(1, Visited),
    digraph:add_edge(G, V1, V2),
    random_tree(G, [V2|Visited], R).

is_connected(G, Root) ->
    Vertices = digraph:vertices(G) -- [Root],
    lists:all(fun(X) ->
                      digraph:get_path(G, Root, X) /= false
              end, Vertices).

print_graph(G) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = lists:sort(digraph:out_neighbours(G, X))
          %% ?DBG("~p -> ~p~n", [X, N])
      end || X <- V ].

get_tree(Root, G) ->
    V = [Root | lists:sort(digraph:vertices(G)) -- [Root]],
    _Out = [ {X, digraph:out_neighbours(G, X)} || X <- V ],
    build_tree(V).

build_tree([_X|_V]) ->
    ok.

print_graph_paths(G, Root) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = digraph:get_path(G, Root, X),
          ?DBG("~p -> ~p~n", [X, N])
      end || X <- V ].

random_tree_test() ->
    [ begin
          DG = digraph:new([acyclic]),
          L = lists:seq(1000, 1015),
          lists:map(digraph:add_vertex(DG, _), L),
          random_tree(DG, L),
          case is_connected(DG, hd(L)) of
              true -> ok;
              false ->
                  print_graph_paths(DG, hd(L)),
                  ?assertEqual(true, false)
          end,
          digraph:delete(DG)
      end || _ <- lists:seq(1, 100) ].

add_random_edge(G) ->
    V = digraph:vertices(G),
    [V1, V2] = dstree_utils:random_pick(2, V),
    case lists:member(V2, digraph:out_neighbours(G, V1)) of
        true -> ok;
        false -> digraph:add_edge(G, V1, V2)
    end.

%% eunit_test_() ->
%%     {foreach, fun () -> setup() end,
%%      fun (State) -> cleanup(State) end,
%%      [{timeout, 100, fun test_random/0},
%%       fun() -> ok end]}.

setup() ->
    ok.
cleanup(_) ->
    ok.

connect(X, Y) ->
    dstree_server:add_edge(X, Y),
    dstree_server:add_edge(Y, X).

basic_test_disabled() ->
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
    dstree:search(v1),
    dstree:search(v2),
    dstree:search(v5),
    dstree:search(v8),
    dstree:search(v4),
    dstree:search(v3),
    timer:sleep(1000),
    [ X ! stop || X <- L ].

report_fun({Action, Origin, Tree}= Args) ->
    ?DBG("~p~n", [Args]).

%%%%%%%%%%%%% old eunit test %%%%%%%%%%%%%%%%%%

test_random() ->
    DG = digraph:new([acyclic]),
    L = [ root | [ list_to_atom(lists:flatten(io_lib:format("v~2..0b", [X]))) || X <- lists:seq(1, 10) ] ],
    lists:map(digraph:add_vertex(DG, _), L),
    random_tree(DG, L),
    STree = get_tree(root, DG),
    %% [ add_random_edge(DG) || _ <- lists:seq(1, 20) ],
    print_graph(DG),
    [ begin
          {ok, P} = dstree_server:start(self(), X, []),
          register(X, P)
      end || X <- L ],
    [ dstree_server:add_edges(X, digraph:out_neighbours(DG, X)) || X <- L ],

    dstree_server:search(root),
    %% [ dstree_server:search(X) || X <- dstree_utils:random_pick(1, L) ],
    timer:sleep(5000),
    [ dstree_server:stop(X) || X <- L ],
    ok.
