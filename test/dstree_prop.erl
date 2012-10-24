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
-export([test_prop_all/0]).
-export([prop_all/0]).

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

test_prop_all() ->
    true = ?QC(dstree_prop:prop_all()).
prop_all() ->
    ?FORALL(X, cgraph(),
            run_test(X)
           ).

setup_nodes(DG) ->
    L = digraph:vertices(DG),
    Pids = start_nodes(L),
    setup_connections(L, DG),
    Pids.

start_nodes(L) ->
    Owner = self(),
    [ begin
          {ok, P} = dstree_server:start(self(), X, [{send_fn, fast_send()},
                                                    {report_fn, report_fun(Owner, make_nop(2))}]),
          erlang:monitor(process, P),
          register(X, P),
          P
      end || X <- L ].

setup_connections(L, DG) ->
    [ dstree_tests:connect(X, Y) || X <- L, Y <- digraph:out_neighbours(DG, X) ].


run_test(CGraph) ->
    Sparse = to_sparse(CGraph),
    DG = sparse_to_digraph(Sparse),
    {connected, true} = {connected, is_connected(DG)},
    L = digraph:vertices(DG),
    VCount = length(L),
    Pids = setup_nodes(DG),

    Start = now(),
    [Root] = dstree_utils:random_pick(1, L),
    dstree_server:search(Root),
    {true, {Root, _} = Tree0} = wait_for(L),
    Time = timer:now_diff(now(), Start),
    true = (Time < (500 * VCount)),

    Tree = tree_to_digraph(Tree0),
    match_graphs(DG, Tree),
    [ dstree_server:stop(X) || X <- L ],
    wait_for_dead(Pids),
    digraph:delete(Tree),
    digraph:delete(DG),
    true.

match_graphs(Original, Result) ->
    OriginalVertices = lists:sort(digraph:vertices(Original)),
    ResultVertices = lists:sort(digraph:vertices(Result)),
    OriginalVertices = ResultVertices,
    [ begin
          ON = ordsets:from_list(digraph:out_neighbours(Original, V)),
          RN = ordsets:from_list(digraph:out_neighbours(Result, V)),
          true = ordsets:is_subset(RN, ON)
      end || V <- ResultVertices ],
    ok.

fast_send() ->
    fun(X, M) ->
            X ! {dstree, M}
    end.

report_fun(Owner, Printer) ->
    fun({done, _Origin, Tree} = Args) ->
            Printer("node has finished: ~p", [Args]),
            Owner ! {ok, Tree}
    end.

make_nop(1) ->
    fun(_) -> ok end;
make_nop(2) ->
    fun(_, _) -> ok end.

wait_for_dead([]) ->
    true;
wait_for_dead([P|R]) ->
    receive
        {'DOWN', _, process, P, _} ->
            wait_for_dead(R)
    after
        1000 ->
            timeout
    end.


wait_for(L) ->
    wait_for(undefined, L).

wait_for(T, []) ->
    {true, T};
wait_for(T, L) ->
    receive
        {ok, Tree} ->
            case T of
                undefined ->
                    wait_for(Tree, tl(L));
                Tree ->
                    wait_for(Tree, tl(L))
            end
    after 5000 ->
            ?DBG("TIMEOUT", []),
            false
    end.

tree_to_digraph({Id, Children}) ->
    DG = digraph:new([acyclic]),
    tree_to_digraph(DG, {Id, Children}),
    true = is_connected(DG),
    DG.

tree_to_digraph(DG, {Id, Children}) ->
    digraph:add_vertex(DG, Id),
    [ tree_to_digraph(DG, Subtree) || Subtree <- Children ],
    [ begin
          digraph:add_edge(DG, X, Id),
          digraph:add_edge(DG, Id, X)
      end || {X, _} <- Children ],
    DG.


%%%===================================================================
%%% Internal functions
%%%===================================================================

is_connected(DG) ->
    1 =:= length(digraph_utils:components(DG)).

sparse_to_digraph(Graph) ->
    DG = digraph:new([cyclic]),
    L = [ {i2a(X), [i2a(E)||E<-Edges]} || {X, Edges} <- lists:zip(lists:seq(1, length(Graph)), Graph) ],
    {Vertices, _} = lists:unzip(L),
    %% ?DBG("Vertices: ~p", [Vertices]),
    [ digraph:add_vertex(DG, V) || V <- Vertices ],
    %% lists:map(digraph:add_vertex(DG, _), Vertices),
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
