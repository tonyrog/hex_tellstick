%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
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
-export([add_event/2, del_event/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	  subs = [] :: [{Ref::reference(),Signal::term()}]
	 }).

%%%===================================================================
%%% API
%%%===================================================================
add_event(Flags, Signal) ->
    gen_server:call(?MODULE, {add_event, Flags, Signal}).

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
    {ok, #state{}}.

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
handle_call({add_event,Flags,Signal}, _From, State) ->
    case tellstick_server:subscribe(Flags) of
	{ok,Ref} ->
	    Subs = [{Ref,Signal}|State#state.subs],
	    lager:debug("added signal ref=~p, flags=~p", [Ref,Flags]),
	    {reply, {ok,Ref}, State#state { subs = Subs }};
	Error ->
	    {reply, Error, State}
    end;
handle_call({del_event,Ref}, _From, State) ->
    case lists:keytake(Ref, 1, State#state.subs) of
	false ->
	    {reply, {error, enoent}, State};
	{value,{_,_Signal}, Subs} ->
	    Res = tellstick_server:unsubscribe(Ref),
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
handle_info({tellstick_event,Ref,EventData}, State) ->
    lager:debug("tellstick_event: ref=~p, event=~p", [Ref, EventData]),
    case lists:keyfind(Ref, 1, State#state.subs) of
	false ->
	    lager:warning("event ~p not found", [EventData]),
	    {noreply, State};
	{_,Signal} ->
	    lager:debug("event signal: ~p", [Signal]),
	    hex_server:event(Signal, EventData),
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    io:format("got info ~p\n", [_Info]),
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

