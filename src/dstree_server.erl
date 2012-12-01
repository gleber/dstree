-module(dstree_server).
-behaviour(gen_server).

-export([start/2, start/3, add_edge/2, add_edges/2, search/1, stop/1]).

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

add_edge(Ref, Neighbor) ->
    gen_server:call(Ref, {add_edge, Neighbor}).

add_edges(Ref, Neighbors) ->
    gen_server:call(Ref, {add_edges, Neighbors}).

%%
%% Callbacks
%%
-record(state, {owner :: pid(),
                dstree :: #dstree{}}).

-define(s, State#state).

init([Owner, Id, Opts]) ->
    {ok, #state{owner = Owner,
                dstree = dstree:new(Id, Opts)}};
init([Owner, Opts]) ->
    {ok, #state{owner = Owner,
                dstree = dstree:new(self(), Opts)}}.

terminate(_Reason, _State) ->
    ok.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({add_edges, N}, _From, State) ->
    {reply, ok, op(add_edges, [N], State)};

handle_call({add_edge, N}, _From, State) ->
    {reply, ok, op(add_edge, [N], State)};

handle_call(_Call, _From, State) ->
    {stop, {unknown_call, _Call}, State}.

handle_cast(search, State) ->
    {noreply, op(search, [], State)};

handle_cast({dstree, Msg}, #state{} = State) ->
    {noreply, op(process, [Msg], State)};

handle_cast(_Cast, State) ->
    {stop, {unknown_cast, _Cast}, State}.

handle_info({dstree, Msg}, #state{} = State) ->
    {noreply, op(process, [Msg], State)};

handle_info(_Msg, State) ->
    {stop, {unknown_info, _Msg}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

op(Op, Args, #state{dstree = DS0} = State) ->
    DStree = apply(dstree, Op, Args ++ [DS0]),
    ?s{dstree = DStree}.
