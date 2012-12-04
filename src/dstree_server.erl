-module(dstree_server).
-behaviour(gen_server).

-export([start/2, start/3,

         add_edge/2, add_edges/2,

         search/1, sync_search/1, sync_search/2, wait/1, wait/2,

         stop/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include_lib("dstree/include/dstree.hrl").

%%
%% API
%%

start(Owner, Opts) ->
    gen_server:start(dstree_server, [Owner, Opts], []).

start(Owner, Id, Opts) ->
    gen_server:start(dstree_server, [Owner, Id, Opts], []).

stop(Ref) ->
    gen_server:call(Ref, stop).

search(Ref) ->
    gen_server:cast(Ref, search).

sync_search(Ref) ->
    sync_search(Ref, 60000).

sync_search(Ref, Timeout) ->
    gen_server:call(Ref, sync_search, Timeout).

wait(Ref) ->
    wait(Ref, 60000).

wait(Ref, Timeout) ->
    gen_server:call(Ref, wait, Timeout).

add_edge(Ref, Neighbor) ->
    gen_server:call(Ref, {add_edge, Neighbor}).

add_edges(Ref, Neighbors) ->
    gen_server:call(Ref, {add_edges, Neighbors}).


%%
%% Callbacks
%%
-record(state, {owner :: pid(),
                waiting = [] :: list(gen:from()),
                dstree :: #dstree{}}).

-define(s, State#state).

init([Owner, Id, Opts0]) ->
    Self = self(),
    Opts = [{report_fn, fun({done, _, T, I}) when I == Id ->
                                Self ! {report, T}
                        end} | Opts0],
    {ok, #state{owner = Owner,
                dstree = dstree:new(Id, Opts)}};

init([Owner, Opts]) ->
    init([Owner, self(), Opts]).

terminate(_Reason, _State) ->
    ok.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(sync_search, From, #state{waiting = W} = State0) ->
    State = State0#state{waiting = [From|W]},
    {noreply, op(search, State)};

handle_call(wait, From, #state{waiting = W, dstree = DST} = State0) ->
    case dstree:get_tree(DST) of
        {ok, Tree} ->
            {reply, {ok, Tree}, State0};
        {error, not_ready} ->
            State = State0#state{waiting = [From|W]},
            {noreply, State}
    end;

handle_call({add_edges, N}, _From, State) ->
    {reply, ok, op(add_edges, [N], State)};

handle_call({add_edge, N}, _From, State) ->
    {reply, ok, op(add_edge, [N], State)};

handle_call({get_tree}, _From, #state{dstree = DST} = State) ->
    {reply, dstree:get_tree(DST), State};

handle_call(_Call, _From, State) ->
    {stop, {unknown_call, _Call}, State}.

handle_cast(search, State) ->
    {noreply, op(search, State)};

handle_cast({dstree, Msg}, #state{} = State) ->
    {noreply, op(process, [Msg], State)};

handle_cast(_Cast, State) ->
    {stop, {unknown_cast, _Cast}, State}.

handle_info({dstree, Msg}, #state{} = State) ->
    {noreply, op(process, [Msg], State)};

handle_info({report, Tree}, #state{waiting = W} = State) ->
    [ gen_server:reply(X, {ok, Tree}) || X <- W ],
    {noreply, State#state{waiting = []}};

handle_info(_Msg, State) ->
    {stop, {unknown_info, _Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

op(Op, State) ->
    op(Op, [], State).

op(Op, Args, #state{dstree = DS0} = State) ->
    DStree = apply(dstree, Op, Args ++ [DS0]),
    ?s{dstree = DStree}.
