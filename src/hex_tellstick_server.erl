%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%
%%% @end
%%% Created :  7 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_tellstick_server).

-behaviour(gen_server).

-include_lib("lager/include/log.hrl").
%% API
-export([start_link/0, stop/0]).
-export([add_event/3, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(sub,
	{
	  ref :: reference(),   %% monitor/ref
	  tref :: reference(),  %% tellstick ref
	  flags :: [{atom(),term()}],
	  signal :: term(),
	  callback :: atom() | function()
	}).

-record(state, {
	  joined = false :: boolean(),
	  subs = [] :: [#sub{}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================
add_event(Flags, Signal,Cb) ->
    gen_server:call(?MODULE, {add_event, self(), Flags, Signal, Cb}).

del_event(Ref) ->
    gen_server:call(?MODULE, {del_event, Ref}).

stop() ->
    gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Joined = hex:auto_join(hex_tellstick),
    {ok, #state{ joined = Joined }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_event,Pid,Flags,Signal,Cb}, _From, State) ->
    case tellstick_server:subscribe(Flags) of
	{ok,TRef} ->
	    Ref = erlang:monitor(process,Pid),
	    Sub = #sub { ref = Ref,
			 tref = TRef,
			 flags = Flags,
			 signal = Signal,
			 callback = Cb },
	    Subs = [Sub|State#state.subs],
	    lager:debug("added signal ref=~p, flags=~p", [Ref,Flags]),
	    {reply, {ok,Ref}, State#state { subs = Subs }};
	Error ->
	    {reply, Error, State}
    end;
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {reply, {error, enoent}, State};
	{value,Sub,Subs} ->
	    erlang:demonitor(Sub#sub.ref, [flush]),
	    Res = tellstick_server:unsubscribe(Sub#sub.tref),
	    {reply, Res, State#state { subs = Subs }}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tellstick_event,TRef,EventData}, State) ->
    lager:debug("tellstick_event: ref=~p, event=~p", [TRef, EventData]),
    case lists:keyfind(TRef, #sub.tref, State#state.subs) of
	false ->
	    lager:warning("event ~p not found", [EventData]),
	    {noreply, State};
	#sub { signal=Signal, callback=Cb } ->
	    lager:debug("event signal: ~p", [Signal]),
	    callback(Cb, Signal, EventData),
	    {noreply, State}
    end;
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    lager:debug("monitor DOWN ~p ~p", [_Pid,_Reason]),
    case lists:keytake(Ref, #sub.ref, State#state.subs) of
	false ->
	    {noeply,State};
	{value,Sub,Subs} ->
	    tellstick_server:unsubscribe(Sub#sub.tref),
	    {noreply,State#state{subs=Subs}}
    end;
handle_info(_Info, State) ->
    lager:debug("unhandled info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    lists:foreach(
      fun({Ref,_Signal}) ->
	      tellstick_server:unsubscribe(Ref)
      end, State#state.subs),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

callback(Cb,Signal,Env) when is_atom(Cb) ->
    Cb:event(Signal, Env);
callback(Cb,Signal,Env) when is_function(Cb, 2) ->
    Cb(Signal,Env).
