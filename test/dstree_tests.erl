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
      {timeout, 5000, fun dstree_prop:test_prop_basic/0},
      {timeout, 5000, fun dstree_prop:test_prop_unstable/0},
      fun() -> ok end]}.


print_graph(G) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = lists:sort(digraph:out_neighbours(G, X)),
          ?DBG("~p -> ~p~n", [X, N])
      end || X <- V ].

print_graph_paths(G, Root) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = digraph:get_path(G, Root, X),
          ?DBG("~p -> ~p~n", [X, N])
      end || X <- V ].

connect(X, Y) ->
    dstree_server:add_edge(X, Y),
    dstree_server:add_edge(Y, X).

