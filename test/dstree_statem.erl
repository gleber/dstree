-module(dstree_statem).

-behaviour(proper_statem).

-export([test/0, test/1]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-export([actor_up/1, actor_up/2,
         actor_down/1,
         actor_connect/2,
         actor_search/1]).

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
                          %% io:format("Cmds: ~p~n", [Cmds]),
                          {H, #state{actors = Actors, graph = G} = S,Res} = run_commands(?MODULE, Cmds),
                          %% {true, Tree} = actor_search(oneof(Actors)),
                          dstree_prop:killall(),
                          ?WHENFAIL(
                             io:format("History: ~w\nState: ~w\nRes: ~w\n",
                                       [H, S, Res]),
                             aggregate(command_names(Cmds), Res =:= ok))
                      end)).


actor_up(N) ->
    Id = i2a(N),
    Owner = self(),
    {ok, P} = dstree_server:start(Owner, Id, [{send_fn, fun dstree_prop:fast_send/3},
                                              %% {report_fn, report_fun(Owner, make_nop(2))}
                                              {report_fn, dstree_prop:report_fun(Owner, fun io:format/2)}
                                             ]),
    erlang:monitor(process, P),
    register(Id, P),
    P.

actor_up(N, Rooted) ->
    actor_up(N),
    actor_connect(i2a(N), Rooted).

actor_down(Id) ->
    dstree_server:stop(Id).

actor_connect(A, B) ->
    dstree_server:add_edge(A, B).

actor_search(A) ->
    dstree_server:search(A),
    receive
        {ok, A, Tree} ->
            {true, Tree}
    after
        5000 ->
            false
    end.

test() ->
    test(100).

test(N) ->
    true = proper:quickcheck(?MODULE:prop_dstree(), N).

initial_state() ->
    #state{graph = 'cgraph'}.

command(#state{actors = [],n = N}) ->
    {call,?MODULE,actor_up,[N+1]};
command(#state{actors = Actors, n = N, root = _Root, graph = G} = _S) ->
    %% ?LET({Key,Value}, weighted_union([{2, elements(State)},
    %%                                {1, {key(),integer()}}]),
    %%      oneof([{call,erlang,put,[Key,Value]},
    %%          {call,erlang,get,[Key]},
    %%          {call,erlang,erase,[Key]}
    %%            ])),
    F = [
         %% [{5, {call,?MODULE,actor_search,[oneof(Actors)]}}],
         {5, {call,?MODULE,actor_up,[N + 1, oneof(Actors)]}},
         {1, {call,?MODULE,actor_down,[oneof(Actors)]}},
         {1, {call,?MODULE,actor_connect,[oneof(Actors), oneof(Actors)]}},
         [{10, {call,?MODULE,actor_search,[oneof(Actors)]}} || dstree_prop:cgraph_is_connected(G) ]
        ],

    frequency(lists:flatten(F)).

precondition(#state{root = Root, graph = G} = State,
             {call,?MODULE,actor_search,[Id]}) ->
    lists:member(Id, State#state.actors) andalso
        Root == undefined andalso
        dstree_prop:cgraph_is_connected(G);
precondition(State,
             {call,?MODULE,actor_down,[Id]}) ->
    lists:member(Id, State#state.actors);
precondition(State,
             {call,?MODULE,actor_connect,[A, B]}) ->
    A /= B andalso
        lists:member(A, State#state.actors) andalso
        lists:member(B, State#state.actors);
precondition(_, _) ->
    true.

%% postcondition(State, {call,erlang,put,[Key,_]}, undefined) ->
%%     not proplists:is_defined(Key, State);
%% postcondition(State, {call,erlang,put,[Key,_]}, Old) ->
%%     {Key,Old} =:= proplists:lookup(Key, State);
%% postcondition(State, {call,erlang,get,[Key]}, Val) ->
%%     {Key,Val} =:= proplists:lookup(Key, State);
%% postcondition(State, {call,erlang,erase,[Key]}, Val) ->
%%     {Key,Val} =:= proplists:lookup(Key, State);
postcondition(#state{}, {call, ?MODULE, actor_search, [Id]}, Tree) ->
    case Tree of
        {true, T} ->
            true;
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
           {call, ?MODULE, actor_search, [Root]}) ->
    State#state{}. %% root = Root
