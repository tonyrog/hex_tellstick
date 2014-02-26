%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Hex Tellstick plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_tellstick).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/2, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal) ->
    hex_tellstick_server:add_event(Flags, Signal).

%%
%%  del_event(Ref::reference()) ->
%%     ok.
del_event(Ref) ->
    hex_tellstick_server:del_event(Ref).
%%
%% output(Flags::[{atom(),term()}], Env::[{atom(),term()}]) ->
%%    ok.
%%
output(Flags, Env) ->
    Protocol = proplists:get_value(protocol,Flags),
    output_protocol(Protocol, Flags, Env).

output_protocol(nexa, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    Value = (proplists:get_value(value, Env) =:= 1),
    tellstick_server:nexa(Unit,Chan,Value,Flags);
output_protocol(nexax, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    Value = case proplists:get_value(dimmer, Flags, false) of
		true ->
		    proplists:get_value(value, Env);
		false ->
		    (proplists:get_value(value, Env) =:= 1)
	    end,
    tellstick_server:nexax(Unit,Chan,Value,Flags);
output_protocol(waveman, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    Value = (proplists:get_value(value, Env) =:= 1),
    tellstick_server:waveman(Unit,Chan,Value,Flags);
output_protocol(sartano, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags,0), %% not used
    Chan = proplists:get_value(channel,Flags),
    Value = (proplists:get_value(value, Env) =:= 1),
    tellstick_server:sartano(Unit,Chan,Value,Flags);
output_protocol(ikea, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags), %% not used
    Chan = proplists:get_value(channel,Flags),
    Value = case proplists:get_value(dimmer, Flags, false) of
		true ->
		    proplists:get_value(value, Env);
		false ->
		    (proplists:get_value(value, Env) =:= 1)
	    end,
    tellstick_server:ikea(Unit,Chan,Value,Flags);

output_protocol(risingsun, Flags, Env) ->
    Unit = proplists:get_value(unit,Flags,0), %% not used
    Chan = proplists:get_value(channel,Flags),
    Value = (proplists:get_value(value, Env) =:= 1),
    tellstick_server:risingsun(Unit,Chan,Value,Flags).

%%
%% init_event(in | out, Flags::[{atom(),term()}]) -> ok | {error,Reason}
%% validate_event is assumed to have been run before init !
init_event(in,_Flags) ->
    ok;
init_event(out,_Flags) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(in, _Flags) ->
    ok;
validate_event(out, Flags) ->
    Protocol = proplists:get_value(protocol,Flags),
    validate_protocol(Protocol, Flags).


validate_protocol(nexa,Flags) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    Dimmer = proplists:get_value(dimmer,Flags,false),
    if  Unit =:= undefined -> {error, {mandatory,[unit]}};
	Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Unit) -> {error, {badarg,unit}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Unit < $A, Unit > $P -> {error, {erange, unit}};
	Chan < 1,  Chan > 16 -> {error, {erange, channel}};
	not is_boolean(Dimmer) -> {error, {erange, dimmer}};
	true -> ok
    end;
validate_protocol(nexax,Flags) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    if  Unit =:= undefined -> {error, {mandatory,[unit]}};
	Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Unit) -> {error, {badarg,unit}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Unit < 0,  Unit > 16#3fffffff -> {error, {erange, unit}};
	Chan < 1,  Chan > 16 -> {error, {erange, channel}};
	true -> ok
    end;
validate_protocol(waveman,Flags) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    if  Unit =:= undefined -> {error, {mandatory,[unit]}};
	Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Unit) -> {error, {badarg,unit}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Unit < $A, Unit > $P -> {error, {erange, unit}};
	Chan < 1,  Chan > 16 -> {error, {erange, channel}};
	true -> ok
    end;
validate_protocol(sartano,Flags) ->
    Chan = proplists:get_value(channel,Flags),
    if  Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Chan < 1,  Chan > 16#3ff -> {error, {erange, channel}};
	true -> ok
    end;
validate_protocol(ikea,Flags) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    Dimmer = proplists:get_value(dimmer,Flags,false),
    Style  = proplists:get_value(style, Flags, smooth),
    if  Unit =:= undefined -> {error, {mandatory,[unit]}};
	Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Unit) -> {error, {badarg,unit}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Unit < $A, Unit > $P -> {error, {erange, unit}};
	Chan < 1,  Chan > 16 -> {error, {erange, channel}};
	not is_boolean(Dimmer) -> {error, {erange, dimmer}};
	Style =/= smooth, Style =/= instant -> {error, {erange, style}};
	true -> ok
    end;
validate_protocol(risingsun,Flags) ->
    Unit = proplists:get_value(unit,Flags),
    Chan = proplists:get_value(channel,Flags),
    if  Unit =:= undefined -> {error, {mandatory,[unit]}};
	Chan =:= undefined -> {error, {mandatory,[channel]}};
	not is_integer(Unit) -> {error, {badarg,unit}};
	not is_integer(Chan) -> {error, {badarg,channel}};
	Unit < 1, Unit  > 4 -> {error, {erange, unit}};
	Chan < 1,  Chan > 4 -> {error, {erange, channel}};
	true -> ok
    end;
validate_protocol(undefined, _Flags) ->
    {error, {mandatory,[protocol]}};
validate_protocol(Protocol, _Flags) ->
    {error, {unsupported_protocol, Protocol}}.
    
