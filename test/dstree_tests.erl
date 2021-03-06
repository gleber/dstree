-module(dstree_tests).

-include_lib("eunit/include/eunit.hrl").

-define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format, [?MODULE, ?LINE] ++ Args)).

-export([connect/2, print_graph/1, graph_to_str/1, print_graph_paths/2]).

setup() ->
    ok.
cleanup(_) ->
    dstree_prop:killall(),
    ok.

double_search_test() ->
    dstree_statem:actor_up(v01),
    dstree_statem:actor_up(v02, v01),
    {ok, TreeA} = dstree_server:sync_search(v01),
    {ok, TreeA2} = dstree_server:wait(v02),
    ?assertMatch({v01, [{v02, []}]}, TreeA),
    ?assertMatch(TreeA, TreeA2),
    dstree_statem:actor_up(v03, v01),
    {ok, TreeB} = dstree_server:sync_search(v02),
    {ok, TreeB2} = dstree_server:wait(v01),
    {ok, TreeB3} = dstree_server:wait(v03),
    ?assertMatch({v02, [{v01, [{v03, []}]}]}, TreeB),
    ?assertMatch(TreeB, TreeB2),
    ?assertMatch(TreeB, TreeB3),
    dstree_prop:killall(),
    ok.

triple_search_test() ->
    %% v01 <-> v02 <-> v03
    %% [{set,{var,1},{call,dstree_statem,actor_up,[v01]}},
    %%  {set,{var,2},{call,dstree_statem,actor_up,[v02,v01]}},
    %%  {set,{var,3},{call,dstree_statem,actor_up,[v03,v02]}},
    %%  {set,{var,4},{call,dstree_statem,actor_search,[[v03,v02,v01],v02]}},
    %%  {set,{var,7},{call,dstree_statem,actor_search,[[v05,v04,v03,v02,v01],v03]}}]
    dstree_prop:killall(),
    dstree_statem:actor_up(v01),
    dstree_statem:actor_up(v02, v01),
    dstree_statem:actor_up(v03, v02),
    {ok, TreeA2} = dstree_server:sync_search(v02),
    {ok, TreeA1} = dstree_server:wait(v01),
    {ok, TreeA3} = dstree_server:wait(v03),
    ?assertMatch({v02, [{v01, []}, {v03, []}]}, TreeA2),
    ?assertMatch(TreeA1, TreeA2),
    ?assertMatch(TreeA3, TreeA2),
    {ok, TreeB3} = dstree_server:sync_search(v03),
    {ok, TreeB1} = dstree_server:wait(v01),
    {ok, TreeB2} = dstree_server:wait(v02),
    ?assertMatch({v03, [{v02, [{v01, []}]}]}, TreeB3),
    ?assertMatch(TreeB1, TreeB3),
    ?assertMatch(TreeB2, TreeB3),
    dstree_prop:killall(),
    ok.


statem_test_() ->
    {timeout, 5000, fun dstree_statem:test/0}.

%% many_dead_1_test_() ->
%%     {timeout, 5000,
%%      fun() ->
%%              A = {[6,5,7,4,9,0,0,3,0],8, {{node,1,11}, {{node,1,10}, {{node,0,9}, {{node,2,8}, {{node,1,7}, {{node,0,6}, {{node,1,5}, {{node,1,4}, {{node,1,3}, {{node,1,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}},
%%              true = dstree_prop:run_once(A)
%%      end}.

%% many_dead_2_test_() ->
%%     {timeout, 5000,
%%      fun() ->
%%              P = {[1,7,9,9,4,10],12,{{node,0,15},{{node,2,14},{{node,0,13},{{node,5,12},{{node,0,11},{{node,2,10},{{node,0,9},{{node,0,8},{{node,2,7},{{node,0,6},{{node,0,5},{{node,2,4},{{node,0,3},{{node,0,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}}}}}},
%%              true = dstree_prop:run_once(P)
%%      end}.

%% more_test_() ->
%%     {timeout, 5000,
%%      fun() ->
%%              P = {[15],5,{{node,7,16},{{node,10,15},{{node,3,14},{{node,11,13},{{node,9,12},{{node,10,11},{{node,3,10},{{node,5,9},{{node,6,8},{{node,5,7},{{node,4,6},{{node,1,5},{{node,3,4},{{node,1,3},{{node,1,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}}}}}}},
%%              true = dstree_prop:run_test(P)
%%      end}.

%% more_2_test_() ->
%%     {timeout, 5000,
%%      fun() ->
%%              P = {[10],0,{{node,1,22},{{node,0,21},{{node,0,20},{{node,0,19},{{node,0,18},{{node,0,17},{{node,0,16},{{node,0,15},{{node,0,14},{{node,0,13},{{node,0,12},{{node,0,11},{{node,3,10},{{node,0,9},{{node,0,8},{{node,0,7},{{node,0,6},{{node,0,5},{{node,0,4},{{node,1,3},{{node,0,2},{{node,0,1},{{node,0,0},cgraph}}}}}}}}}}}}}}}}}}}}}}}},
%%              true = dstree_prop:run_test(P)
%%      end}.

%% proper_test_() ->
%%     {foreach, fun () -> setup() end,
%%      fun (State) -> cleanup(State) end,
%%      [
%%       {timeout, 5000, fun dstree_prop:test_prop_basic/0},
%%       {timeout, 5000, fun dstree_prop:test_prop_unstable/0},
%%       fun() -> ok end]}.

print_graph(G) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = lists:sort(digraph:out_neighbours(G, X)),
          io:format("~p -> ~p~n", [X, N])
      end || X <- V ].

graph_to_str(G) ->
    V = lists:sort(digraph:vertices(G)),
    R = [ {X, lists:sort(digraph:out_neighbours(G, X))} || X <- V ],
    digraph:delete(G),
    [ io_lib:format("~p -> ~p~n", [X, N])
      || {X, N} <- R ].

print_graph_paths(G, Root) ->
    V = lists:sort(digraph:vertices(G)),
    [ begin
          N = digraph:get_path(G, Root, X),
          io:format("~p -> ~p~n", [X, N])
      end || X <- V ].

connect(X, Y) ->
    dstree_server:add_edge(X, Y),
    dstree_server:add_edge(Y, X).
