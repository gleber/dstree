-module(bug_case_statem).

-behaviour(proper_statem).

-export([test/0]).
-export([initial_state/0, command/1, precondition/2, postcondition/3,
         next_state/3]).

-export([actor_up/1,
         actor_search/2]).

-include_lib("proper/include/proper.hrl").


-record(state, {n = 0 :: integer(),
                actors = [] :: list(atom())}).

prop_bug() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(begin
                          {H, #state{} = S, Res} = run_commands(?MODULE, Cmds),
                          dstree_prop:killall(),
                          ?WHENFAIL(
                             io:format("History: ~p~nCommands: ~p~nState: ~p~nRes: ~w\n",
                                       [H, Cmds, S, Res]),
                             aggregate(command_names(Cmds), Res =:= ok))
                      end)).


actor_up(Id) ->
    Id.

actor_search(_Actors, _A) ->
    length(_Actors) < 5.

test() ->
    test(100).

test(N) ->
    true = proper:quickcheck(?MODULE:prop_bug(), N).

initial_state() ->
    #state{}.

command(#state{actors = [],n = N}) ->
    {call,?MODULE,actor_up,[N+1]};
command(#state{actors = Actors, n = N} = _S) ->
    frequency([
               {5,  {call,?MODULE,actor_up,[N + 1]}},
               {10, {call,?MODULE,actor_search,[Actors, oneof(Actors)]}}
              ]).

precondition(#state{} = State,
             {call,?MODULE,actor_search,[_Actors, Id]}) ->
    lists:member(Id, State#state.actors);
precondition(State,
             {call,?MODULE,actor_up,[A]}) ->
    not lists:member(A, State#state.actors);
precondition(_, _) ->
    true.

postcondition(#state{actors = Actors1}, {call, ?MODULE, actor_search, [Actors2, _Id]}, Res) ->
    Actors1 == Actors2;

postcondition(_, _, _) ->
    true.

next_state(#state{actors = Actors0, n = SN} = State, _Var,
           {call,?MODULE,actor_up,[N]}) ->
    State#state{actors = [N | Actors0], n = SN + 1};

next_state(#state{} = State, _Var,
           {call, ?MODULE, actor_search, [Actors, _Id]}) ->
    State#state{}.
