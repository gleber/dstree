-module(dstree_statem).

-behaviour(proper_statem).

-compile({parse_transform, cut}).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-export([actor_up/1, actor_up/2,
         actor_down/1,
         actor_connect/2,
         actor_search/2]).

-include_lib("proper/include/proper.hrl").

-import(dstree_prop, [i2a/1,
                      fast_send/3,
                      report_fun/2]).

-record(state, {n = 0 :: integer(),
                actors = [] :: list(atom()),
                root = undefined :: 'undefined' | atom(),
                start = undefined :: 'undefined' | os:timestamp(),
                graph :: dstree_tests:cgraph()}).

prop_dstree() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(begin
                          {H, #state{actors = Actors, graph = G} = _S, Res} = run_commands(?MODULE, Cmds),
                          dstree_prop:killall(),
                          ?WHENFAIL(
                             io:format("History: ~p~nCommands: ~p~nGraph: ~s~nActors: ~p~nRes: ~w\n",
                                       [H, Cmds, dstree_tests:graph_to_str(dstree_prop:cgraph_to_digraph(G)), Actors, Res]),
                             aggregate(command_names(Cmds), Res =:= ok))
                      end)).

prop_parallel_dstree() ->
    ?FORALL(Cmds, proper_statem:parallel_commands(?MODULE),
            ?TRAPEXIT(begin
                          {Sequential, Parallel, Res} = proper_statem:run_parallel_commands(?MODULE, Cmds),
                          dstree_prop:killall(),
                          ?WHENFAIL(
                             io:format("Cmds: ~p~nSeq: ~p~nParallel: ~p~nRes: ~w\n",
                                       [Cmds, Sequential, Parallel, Res]),
                             Res =:= ok)
                      end)).


actor_up(Id) ->
    Owner = self(),
    {ok, P} = dstree_server:start(Owner, Id, [{send_fn, fun dstree_prop:fast_send/3},
                                              {expand_neighbors, false}]), %% it has to be false for tests, otherwise it's nearly impossible to compare graphs
    erlang:monitor(process, P),
    register(Id, P),
    Id.

actor_up(Id, Rooted) ->
    actor_up(Id),
    actor_connect(Id, Rooted),
    Id.

actor_down(Id) ->
    dstree_server:stop(Id).

actor_connect(A, B) ->
    dstree_server:add_edge(B, A),
    dstree_server:add_edge(A, B).

actor_search(_Actors, A) ->
    {ok, Tree} = dstree_server:sync_search(A, 1000),
    {true, Tree}.

test() ->
    test(100).

test(N) ->
    true = proper:quickcheck(?MODULE:prop_parallel_dstree(), N).

initial_state() ->
    #state{graph = 'cgraph'}.

command(#state{actors = [],n = N}) ->
    {call,?MODULE,actor_up,[i2a(N+1)]};
command(#state{actors = Actors, n = N, root = _Root, graph = G} = _S) ->
    F = [
         {5,   {call,?MODULE,actor_up,[i2a(N + 1), oneof(Actors)]}},
         {1,   {call,?MODULE,actor_down,[oneof(Actors)]}},
         {1,   {call,?MODULE,actor_connect,[oneof(Actors), oneof(Actors)]}},
         [{10, {call,?MODULE,actor_search,[Actors, oneof(Actors)]}} || dstree_prop:cgraph_is_connected(G) ]
        ],

    frequency(lists:flatten(F)).

precondition(#state{root = _Root, graph = G} = State,
             {call,?MODULE,actor_search,[_Actors, Id]}) ->
    lists:member(Id, State#state.actors) andalso
        dstree_prop:cgraph_is_connected(G);
precondition(State,
             {call,?MODULE,actor_down,[Id]}) ->
    lists:member(Id, State#state.actors);
precondition(State,
             {call,?MODULE,actor_connect,[A, B]}) ->
    A /= B andalso
        lists:member(A, State#state.actors) andalso
        lists:member(B, State#state.actors);
precondition(State,
             {call,?MODULE,actor_up,[A, Root]}) ->
    Root /= A andalso
        lists:member(Root, State#state.actors) andalso
        not lists:member(A, State#state.actors);
precondition(_, _) ->
    true.

postcondition(#state{actors = Actors, graph = G}, {call, ?MODULE, actor_search, [_Actors, Id]}, Res) ->
    case Res of
        {true, Tree} ->
            Resulting = dstree_prop:tree_to_digraph(Tree),
            Graph = dstree_prop:cgraph_to_digraph(G),
            M = dstree_prop:match_graphs(Graph, Resulting),
            digraph:delete(Graph),
            digraph:delete(Resulting),
            case M of
                false ->
                    false;
                true ->
                    Results = [ {Id, Tree} | [ begin
                                                  {ok, T} = dstree_server:wait(X, 1000),
                                                  {X, T}
                                              end || X <- Actors -- [Id] ]],
                    case lists:usort(lists:map(element(2, _), Results)) of
                        [Tree] ->
                            true;
                        _ ->
                            io:format("Some actors return bad values: ~p~n", [Results]),
                            false
                    end
            end;
        false ->
            false
    end;

postcondition(_, _, _) ->
    true.

next_state(#state{actors = Actors0, n = SN, graph = G} = State, _Var,
           {call,?MODULE,actor_up,[N]}) ->
    Id = i2a(N),
    G2 = {{add_node, Id}, G},
    State#state{actors = [Id | Actors0], n = SN + 1, graph = G2};

next_state(#state{actors = Actors0, n = SN, graph = G} = State, _Var,
           {call,?MODULE,actor_up,[N, Rooted]}) ->
    Id = i2a(N),
    G2 = {{add_node, Rooted, Id}, G},
    State#state{actors = [Id | Actors0], n = SN + 1, graph = G2};

next_state(#state{actors = Actors0, graph = G} = State, _Var,
           {call,?MODULE,actor_down,[Id]}) ->
    G2 = {{del_node, Id}, G},
    State#state{actors = lists:delete(Id, Actors0), graph = G2};

next_state(#state{graph = G} = State, _Var,
           {call,?MODULE,actor_connect,[A, B]}) ->
    G2 = {{add_edge, A, B}, G},
    State#state{graph = G2};

next_state(#state{} = State, _Var,
           {call, ?MODULE, actor_search, [_, _Root]}) ->
    State#state{}. %% root = Root
