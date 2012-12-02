-module(dstree_statem).

-behaviour(proper_statem).

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
                          {H, #state{actors = Actors, graph = G} = S, Res} = run_commands(?MODULE, Cmds),
                          dstree_prop:killall(),
                          ?WHENFAIL(
                             io:format("History: ~p~nCommands: ~p~nGraph: ~s~nRes: ~w\n",
                                       [H, Cmds, dstree_tests:graph_to_str(dstree_prop:cgraph_to_digraph(G)), Res]),
                             aggregate(command_names(Cmds), Res =:= ok))
                      end)).


actor_up(Id) ->
    Owner = self(),
    {ok, P} = dstree_server:start(Owner, Id, [{send_fn, fun dstree_prop:fast_send/3},
                                              %% {report_fn, report_fun(Owner, make_nop(2))}
                                              {report_fn, dstree_prop:report_fun(Owner, fun io:format/2)}
                                             ]),
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

actor_search(Actors, A) ->
    dstree_server:search(A),
    receive
        {ok, A, Tree} ->
            Other = [ receive {ok, X, _} -> true after 1000 -> false end || X <- Actors -- [A] ],
            case lists:usort(Other) of
                [true] ->
                    {true, Tree};
                _ ->
                    false
            end
    after
        1000 ->
            false
    end.

test() ->
    test(100).

test(N) ->
    true = proper:quickcheck(?MODULE:prop_dstree(), N).

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

precondition(#state{root = Root, graph = G} = State,
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
        {true, T} ->
            Resulting = dstree_prop:tree_to_digraph(T),
            Graph = dstree_prop:cgraph_to_digraph(G),
            M = dstree_prop:match_graphs(Graph, Resulting),
            digraph:delete(Graph),
            digraph:delete(Resulting),
            M;
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
           {call, ?MODULE, actor_search, [_, Root]}) ->
    State#state{}. %% root = Root
