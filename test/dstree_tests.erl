-module(dstree_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format, [?MODULE, ?LINE] ++ Args)).

-export([connect/2, print_graph/1, print_graph_paths/2]).

setup() ->
    ok.
cleanup(_) ->
    ok.

proper_test_() ->
    {foreach, fun () -> setup() end,
     fun (State) -> cleanup(State) end,
     [
      {timeout, 5000, fun() ->
                              A = {[6,5,7,4,9,0,0,3,0],8, {{node,1,11}, {{node,1,10}, {{node,0,9}, {{node,2,8}, {{node,1,7}, {{node,0,6}, {{node,1,5}, {{node,1,4}, {{node,1,3}, {{node,1,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}},
                              true = dstree_prop:run_once(A)
                      end},

      {timeout, 5000, fun() ->
                              B = {[1,7,9,9,4,10],12,{{node,0,15},{{node,2,14},{{node,0,13},{{node,5,12},{{node,0,11},{{node,2,10},{{node,0,9},{{node,0,8},{{node,2,7},{{node,0,6},{{node,0,5},{{node,2,4},{{node,0,3},{{node,0,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}}}}}},
                              true = dstree_prop:run_once(B)
                      end},
      
      {timeout, 5000, fun dstree_prop:test_prop_basic/0},
      {timeout, 5000, fun dstree_prop:test_prop_unstable/0},
      fun() -> ok end]}.


print_graph(G) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = lists:sort(digraph:out_neighbours(G, X)),
          io:format("~p -> ~p~n", [X, N])
      end || X <- V ].

print_graph_paths(G, Root) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = digraph:get_path(G, Root, X),
          io:format("~p -> ~p~n", [X, N])
      end || X <- V ].

connect(X, Y) ->
    dstree_server:add_edge(X, Y),
    dstree_server:add_edge(Y, X).

