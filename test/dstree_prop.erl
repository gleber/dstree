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
-export([test_prop_basic/0, prop_basic/0]).
-export([test_prop_unstable/0, prop_unstable/0]).

-compile(export_all).

-include_lib("dstree/include/dstree.hrl").
-include_lib("dstree/include/dstree_dev.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DBG(Format, Args), ok).

%% -define(DBG(Format, Args), ((fun() ->
%%                                      __Now = now(),
%%                                      {_, _, __MS} = __Now,
%%                                      {_, {__Hh, __Mm, __Ss}} = calendar:now_to_local_time(__Now),
%%                                      io:fwrite(user, "[~2..0b:~2..0b:~2..0b.~3..0b] -- " ++ Format ++ "~n", [__Hh, __Mm, __Ss, __MS div 1000] ++ Args)
%%                              end)())).

-define(QC(Arg), proper:quickcheck(Arg, [{numtests, 500},
                                         {constraint_tries, 100},
                                         {on_output, fun printer/2}])).

-type pos() :: non_neg_integer().
-type cgraph() :: {digraph, digraph()} | {{node, pos()}, cgraph()} | {{edge, pos(), pos()}, cgraph()}.

%%%===================================================================
%%% API
%%%===================================================================

test_prop_basic() ->
    true = ?QC(dstree_prop:prop_basic()).
prop_basic() ->
    ?FORALL(X, cgraph(),
            run_test(X)
           ).

test_prop_unstable() ->
    true = ?QC(dstree_prop:prop_unstable()).
prop_unstable() ->
    ?TRAPEXIT(?FORALL(X, cgraph_faulty(),
                      run_test(X)
                     )).

printer(F, A) ->
    io:format(user, F, A).

setup_nodes(DG) ->
    L = digraph:vertices(DG),
    Pids = start_nodes(L),
    setup_connections(L, DG),
    Pids.

fast_send(_Id, X, M) ->
    ?DBG("at ~p send ~p to ~p~n", [_Id, M, X]),
    catch (X ! {dstree, M}).

report_fun(Owner, _Printer) ->
    fun({done, _Origin, Tree, Id} = _Args) ->
            %% _Printer("~p node has finished: ~p~n", [Id, _Args]),
            Owner ! {ok, Id, Tree}
    end.

start_nodes(L) ->
    Owner = self(),
    [ begin
          {ok, P} = dstree_server:start(self(), X, [{send_fn, fun fast_send/3},
                                                    %% {report_fn, report_fun(Owner, make_nop(2))}
                                                    {report_fn, report_fun(Owner, fun io:format/2)}
                                                   ]),
          erlang:monitor(process, P),
          register(X, P),
          P
      end || X <- L ].

setup_connections(L, DG) ->
    [ dstree_tests:connect(X, Y) || X <- L, Y <- digraph:out_neighbours(DG, X) ].

run_test(Problem) ->
    lists:all(fun run_once/1, lists:duplicate(10, Problem)).

run_once({Kill0, Root0, CGraph} = _Problem) ->
    Kill = lists:usort(Kill0),
    Root = i2a(Root0),
    ?DBG("Root: ~p~n", [Root]),
    DG = cgraph_to_digraph(CGraph),
    {connected, true} = {connected, is_connected(DG)},
    %% dstree_tests:print_graph(DG),
    L = digraph:vertices(DG),
    Pids = setup_nodes(DG),
    Killable = lists:map(i2a(_), Kill),
    Left = L -- Killable,

    Start = now(),
    [ dstree_server:stop(V) || V <- Killable ],
    ?DBG("K: ~p~n", [Killable]),

    dstree_server:search(Root),
    Res = wait_for(Left, 3000 * (length(Kill)+1)),
    RR =
        case Res of
            {true, {Root, _} = Tree0} ->
                _Time = timer:now_diff(now(), Start),
                %% {true, fast_test} = {(Time < (5000 * VCount)), fast_test},
                Tree = tree_to_digraph(Tree0),
                Match = match_graphs(DG, Killable, Tree),
                digraph:delete(Tree),
                Match;
            _ ->
                false
        end,
    [ dstree_server:stop(X) || X <- Left ],
    wait_for_dead(Pids),
    digraph:delete(DG),
    [ catch exit(whereis(dstree_prop:i2a(I)), kill) || I <- lists:seq(0, 1000) ],
    RR.

is_critical(DG, V) ->
    Vs = digraph:vertices(DG),
    DG2 = digraph_utils:subgraph(DG, Vs -- [V]),
    R = is_connected(DG2),
    digraph:delete(DG2),
    not R.

match_graphs(Original, Result) ->
    match_graphs(Original, [], Result).

match_graphs(Original, Killed, Result) ->
    OriginalVertices = lists:sort(digraph:vertices(Original)) -- Killed,
    ResultVertices = lists:sort(digraph:vertices(Result)),
    OriginalVertices = ResultVertices,
    Res =
        [ begin
              ON = ordsets:from_list(digraph:out_neighbours(Original, V)),
              RN = ordsets:from_list(digraph:out_neighbours(Result, V)),
              true = ordsets:is_subset(RN, ON)
          end || V <- ResultVertices ],
    [true] == lists:usort(Res).

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
        5000 ->
            timeout
    end.

wait_for(L) ->
    wait_for(L, 5000).

wait_for(L, Timeout) ->
    wait_for(undefined, L, Timeout).

wait_for(T, [], _) ->
    {true, T};
wait_for(T, L, Timeout) ->
    receive
        {ok, Id, Tree} ->
            case T of
                undefined ->
                    wait_for(Tree, L -- [Id], Timeout);
                Tree ->
                    wait_for(Tree, L -- [Id], Timeout)
            end
    after Timeout ->
            ?DBG("TIMEOUT ~p", [L]),
            false
    end.

tree_to_digraph({Id, Children}) ->
    DG = digraph:new([acyclic]),
    tree_to_digraph(DG, {Id, Children}),
    {true, is_connected} = {is_connected(DG), is_connected},
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

%%%%%%%%%%%%%%% generators %%%%%%%%%%%%%%%%%%
i2a(X) -> index_to_atom(X).
index_to_atom(X) ->
    list_to_atom(lists:flatten(io_lib:format("v~2..0b", [X]))).

-spec cgraph_to_digraph(cgraph()) -> digraph().
cgraph_to_digraph(CG) ->
    {digraph, DG} = cgraph_to_digraph0(CG),
    DG.

cgraph_to_digraph0(cgraph) ->
    {digraph, digraph:new()};
cgraph_to_digraph0({{node, Root, Id}, {digraph, DG}}) ->
    digraph:add_vertex(DG, i2a(Id)),
    digraph:add_edge(DG, i2a(Root), i2a(Id)),
    digraph:add_edge(DG, i2a(Id), i2a(Root)),
    {digraph, DG};
cgraph_to_digraph0({{edge, A, B}, {digraph, DG}}) ->
    digraph:add_edge(DG, i2a(A), i2a(B)),
    digraph:add_edge(DG, i2a(B), i2a(A)),
    {digraph, DG};
cgraph_to_digraph0({Op, Chain}) ->
    cgraph_to_digraph0({Op, cgraph_to_digraph0(Chain)}).

cgraph_with_dead_is_connected({Kill, _Root, CG}) ->
    DG = cgraph_to_digraph(CG),
    [ digraph:del_vertex(DG, i2a(K)) || K <- Kill ],
    Res = is_connected(DG),
    digraph:delete(DG),
    Res.


pos(N) ->
    integer(0, N).

cgraph_faulty() ->
    ?SIZED(S, ?SUCHTHAT(X, cgraph_root_with_dead(S), cgraph_with_dead_is_connected(X))).

cgraph_root_with_dead(S) ->
    ?SUCHTHAT(X, cgraph_with_dead(S), root_not_killed(X)).

root_not_killed({Kill, Root, _CG}) ->
    not lists:member(Root, Kill).

cgraph_with_dead(S) ->
    %%{resize(trunc(S * (2/3)), list(pos(S))), pos(S), cgraph(S)}.
    ?LET(SS, pos(S), {vector(SS, pos(S)), pos(S), cgraph(S)}).

cgraph() ->
    ?SIZED(S, clean_cgraph(S)).

clean_cgraph(S) ->
    {[], pos(S), cgraph(S)}.

rand_list(K, Max) ->
    vector(K, pos(Max)).

cgraph(0) ->
    {{'node', 0, 0}, 'cgraph'};
cgraph(S) ->
    frequency([
               {10, ?LAZY({{'node', pos(S-1), S}, cgraph(S-1)})},
               {0, ?LAZY({{'edge', pos(S), pos(S)}, cgraph(S)})}
              ]).
