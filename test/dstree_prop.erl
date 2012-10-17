%%%-------------------------------------------------------------------
%%% @author Paul Peregud <paulperegud@gmail.com>
%%% @copyright (C) 2012, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2012 by Paul Peregud <paulperegud@gmail.com>
%%%-------------------------------------------------------------------
-module(dstree_prop).

%% API
-export([test_prop_simple/0, test_prop_all/0]).
-export([prop_simple/0, prop_all/0]).

-compile(export_all).

-include_lib("dstree/include/dstree.hrl").
-include_lib("dstree/include/dstree_dev.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DBG(Format, Args), io:fwrite(user, "~p:~p -- " ++ Format ++ "~n", [?MODULE, ?LINE] ++ Args)).
-define(QC(Arg), proper:quickcheck(Arg, [{numtests, 500}, {on_output, dstree_tests:printer()}])).

-type pos() :: non_neg_integer().
-type sparse_graph() :: list(list(pos())).
-type cgraph() :: {graph, sparse_graph()} | {{node, pos()}, cgraph()} | {{edge, pos(), pos()}, cgraph()}.

%%%===================================================================
%%% API
%%%===================================================================

test_prop_simple() -> 
    true = ?QC(dstree_prop:prop_simple()).
prop_simple() ->
    ?FORALL(X, boolean(),
            X =:= (X andalso X)
      ).

test_prop_all() -> 
    true = ?QC(dstree_prop:prop_all()).
prop_all() ->
    ?FORALL(X, cgraph(),
            run_test(X)
           ).

run_test(CGraph) ->
    Sparse = to_sparse(CGraph),
    DG = sparse_to_digraph(Sparse),
    {connected, true} = {connected, is_connected(DG)},
    L = digraph:vertices(DG),
    dstree_tests:print_graph(DG),
    Owner = self(),
    [ begin
          {ok, P} = dstree_server:start(self(), X, [{send_fn, fast_send()}, {report_fn, report_fun(Owner, make_nop(2))}]),
          register(X, P)
      end || X <- L ],
    [ dstree_tests:connect(X, Y) || X <- L, Y <- digraph:out_neighbours(DG, X) ],
    [ dstree_server:search(X) || X <- dstree_utils:random_pick(1, L) ],
    Res = wait_for(L),
    [ dstree_server:stop(X) || X <- L ],
    timer:sleep(100),
    Res.

fast_send() ->
    fun(X, M) ->
            X ! {dstree, M}
    end.

report_fun(Owner, Printer) ->
    fun({done, _Origin, _Tree} = Args) ->
            Printer("node has finished: ~p", [Args]),
            Owner ! ok
    end.

make_nop(1) ->
    fun(_) -> ok end;
make_nop(2) ->
    fun(_, _) -> ok end.

wait_for([]) ->
    true;
wait_for(L) ->
    receive
        ok ->
            wait_for(tl(L))
    after 5000 ->
            ?DBG("TIMEOUT", []),
            false
    end.
              

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_connected(DG) ->
    1 =:= length(digraph_utils:components(DG)).

sparse_to_digraph(Graph) ->
    DG = digraph:new([cyclic]),
    L = [ {i2a(X), [i2a(E)||E<-Edges]} || {X, Edges} <- lists:zip(lists:seq(1, length(Graph)), Graph) ],
    {Verticies, _} = lists:unzip(L),
    %% ?DBG("Verticies: ~p", [Verticies]),
    [ digraph:add_vertex(DG, V) || V <- Verticies ],
    %% lists:map(digraph:add_vertex(DG, _), Verticies),
    [ begin
          %% ?DBG("edge: ~p", [{V, E}]),
          digraph:add_edge(DG, V, E),
          %% ?DBG("edge: ~p", [{E, V}]),
          digraph:add_edge(DG, E, V)
      end
      || {V, Edges} <- L, E <- Edges ],
    DG.

add_node(Int, Graph) ->
    Target = Int rem length(Graph) + 1,
    Graph ++ [[Target]].

add_edge(_, _, [X]) ->
    [X];
add_edge(A, A, Graph) ->
    Graph;
add_edge(A, B, Graph) ->
    %% ?DBG("dstree_prop:add_edge(~p, ~p, ~p)", [A, B, Graph]),
    TA = (A rem length(Graph)) + 1,
    TB = (B rem length(Graph)) + 1,
    G1 = store(TA, TB, Graph),
    store(TB, TA, G1).

store(Where, What, List) ->
    %% ?DBG("dstree_prop:store(~p, ~p, ~p)", [Where, What, List]),
    A = lists:sublist(List, Where - 1),
    El = lists:nth(Where, List),
    C = lists:nthtail(Where, List),
    B = [lists:usort([What | El])],
    Res = A ++ B ++ C,
    %% ?DBG("~p ~p ~p", [A, B, C]),
    Res.

-spec to_sparse(cgraph()) -> sparse_graph().
to_sparse(Graph) ->
    {graph, G} = to_sparse0(Graph),
    G.

-spec to_sparse0(cgraph()) -> cgraph().
to_sparse0({{node, Int}, {graph, Graph}}) ->
    {graph, add_node(Int, Graph)};
to_sparse0({{edge, A, B}, {graph, Graph}}) ->
    {graph, add_edge(A, B, Graph)};
to_sparse0({Op, Chain}) ->
    to_sparse0({Op, to_sparse0(Chain)}).

i2a(X) -> index_to_atom(X).
index_to_atom(X) ->
    list_to_atom(lists:flatten(io_lib:format("v~2..0b", [X]))).

%%%%%%%%%%%%%%% generators %%%%%%%%%%%%%%%%%%

pos() ->
    non_neg_integer().

cgraph() ->
    ?SIZED(S, cgraph(S)).

cgraph(0) ->
    {'graph', [[]]};
cgraph(S) ->
    frequency([
               {3, ?LAZY({{'node', pos()}, cgraph(S-1)})},
               {1, ?LAZY({{'edge', pos(), pos()}, cgraph(S-1)})}
              ]).

