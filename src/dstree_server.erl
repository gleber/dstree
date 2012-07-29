-module(dstree_server).
-behaviour(gen_server).

-export([start/2, search/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include_lib("dstree/include/dstree.hrl").

-record(state, {owner :: pid(),
                dstree :: #dstree{}}).

-define(s, State#state).

start(Owner, Neighbors) ->
    gen_server:start_link(dstree_server, [Owner, Neighbors]).

search(Ref) ->
    gen_server:cast(Ref, search).

init([Owner, Neighbors]) ->
    {ok, #state{owner = Owner,
                dstree = dstree:new(self(), Neighbors)}}.

terminate(_Reason, _State) ->
    ok.

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

op(Op, Args, State) ->
    DStree0 = ?s.dstree,
    DStree = apply(dstree, Op, Args ++ [DStree0]),
    ?s{dstree = DStree}.
