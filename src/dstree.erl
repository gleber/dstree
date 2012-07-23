-module(dstree).

-export([new/2, process/2]). 

-include_lib("eunit/include/eunit.hrl").

-export([server/2, search/1, dfs_loop/1]).

-record(dstree,
        {id,
         status = initial,
         owner,
         origin,
         parent,
         neighbors = [],
         children = [],
         dbg_fun,
         report_fun,
         send_fun = fun default_send/2}).

default_send(X, M) ->
    erlang:send_after(crypto:rand_uniform(1, 100), X, {dstree, M}).

random_test() ->
    L = [v1,v2,v3,v4,v5,v6,v7,v8],
    %%[ register(X, spawn(dstree, test, [X, dstree_utils:random_pick(L)])) || X <- L ],
    [ register(X, spawn(dstree, server, [X, L])) || X <- L ],
    [ search(X) || X <- dstree_utils:random_pick(1, L) ],
    timer:sleep(1000),
    [ X ! stop || X <- L ].

basic_test() ->
    L = [v1,v2,v3,v4,v5,v6,v7,v8],
    register(v1,
             spawn(dstree, server, [v1, [v2, v3]])),
    register(v2,
             spawn(dstree, server, [v2, [v3, v5, v4]])),
    register(v3,
             spawn(dstree, server, [v3, [v2, v4, v6]])),
    register(v4,
             spawn(dstree, server, [v4, [v5, v6, v2, v3]])),
    register(v5,
             spawn(dstree, server, [v5, [v2, v4, v6, v7, v8]])),
    register(v6,
             spawn(dstree, server, [v6, [v4, v3, v5, v8]])),
    register(v7,
             spawn(dstree, server, [v7, [v5, v1]])),
    register(v8,
             spawn(dstree, server, [v8, [v5, v6]])),
    search(v1),
    search(v2),
    search(v5),
    search(v8),
    search(v4),
    search(v3),
    timer:sleep(1000),
    [ X ! stop || X <- L ].
%% v1 ! {forward, v1, [], v1},
%% v2 ! {forward, v2, [], v2},
%% v5 ! {forward, v5, [], v5}.
%%,    v5 ! stop.

server(I, Neighbors) ->
    dfs_loop(dstree:new(I, Neighbors)).

new(I, Neighbors) ->
    #dstree{status = initial,
            owner = self(),
            id = I,
            parent = I,
            neighbors = Neighbors,
            children = []}.

dfs_loop(State) ->
    receive
        stop ->
            ok;
        {dstree, Msg} ->
            dstree:dfs_loop(dstree:process(Msg, State))
    after 500 ->
            dstree:dfs_loop(State)
    end.

wait(X) ->
    Ref = make_ref(),
    X ! {Ref,}

search(X) ->
    X ! {dstree, {forward, X, [], X}}.

m(X, Y) ->
    lists:usort(X ++ Y).

process(Msg, #dstree{status = initial,
                     id = I,
                     parent = Parent,
                     neighbors = Neighbors} = State) ->
    case Msg of
        {forward, Sender, Visited, Origin} ->
            io:fwrite("~s got forward (~s)\n", [I, Origin]),
            check(State#dstree{status = process,
                               parent = Sender,
                               neighbors = m(Neighbors, Visited),
                               origin = Origin,
                               children = []},
                  [I | Visited]);
        report ->
            io:fwrite("~s report: parent - ~s | ~p\n", [I, Parent, lists:usort(Neighbors)]),
            State2 = broadcast(report, State),
            State2;
        _Msg ->
            io:fwrite("~s unkown message ~p~n", [I, _Msg]),
            State
    end;

process(Msg, #dstree{status = waiting,
                     id = I,
                     parent = Parent,
                     neighbors = Neighbors,
                     origin = Origin} = State) ->
    case Msg of
        report ->
            io:fwrite("~s report: parent - ~s | ~p\n", [I, Parent, lists:usort(Neighbors)]),
            State2 = broadcast(report, State),
            State2;
        {return, Visited, X} when X == Origin ->
            io:fwrite("~s got return (~s)\n", [I, Origin]),
            check(State, Visited);
        {forward, Sender, Visited, MOrigin} ->
            if MOrigin > Origin ->
                    io:fwrite("~s got forward (~s)\n", [I, MOrigin]),
                    check(State#dstree{parent = Sender,
                                       neighbors = m(Neighbors,Visited),
                                       origin = MOrigin,
                                       children = []},
                          [I | Visited]);
               true ->
                    io:fwrite("~s ignores ~s (~s)\n", [I, Sender, Origin]),
                    State#dstree{neighbors = m(Neighbors,Visited)}
            end;
        {finished, X} when X == Origin ->
            broadcast({finished, Origin}, State),
            State#dstree{status = initial}
    end.

check(#dstree{id = I,
              parent = Parent,
              neighbors = Neighbors,
              origin = Origin,
              children = Children} = State,
      Visited) ->
    Unvisited = Neighbors -- Visited,
    case Unvisited of
        [] ->
            if I == Parent ->
                    State2 = broadcast({finished, Origin}, State),
                    State3 = do_send(report, I, State2),
                    State3#dstree{status = initial};
               true ->
                    State2 = do_send({return, Visited, Origin}, Parent, State),
                    State2#dstree{status = waiting}
            end;
        [J | _] ->
            State2 = do_send({forward, I, Visited, Origin}, J, State),
            State2#dstree{status = waiting,
                          children = [J | Children]}
    end.


broadcast(M, #dstree{children = Children} = State) ->
    lists:foldl(fun(X, S) ->
                        do_send(M, X, S)
                end, State, shuffle(Children)).

do_send(M, X, #dstree{send_fun = SF} = State) ->
    SF(X, M),
    State.

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).
