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
%%%    Hex Tellstick plugin 
%%% @end
%%% Created :  24 Feb 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(hex_tellstick).

-behaviour(hex_plugin).

-export([validate_event/2, 
	 init_event/2,
	 add_event/3, 
	 del_event/1, 
	 output/2]).

%%
%%  add_event(Flags::[{atom(),term()}, Signal::signal()) ->    
%%     {ok, Ref:reference()} | {error, Reason}
%%
add_event(Flags, Signal, Cb) ->
    hex_tellstick_server:add_event(Flags, Signal, Cb).

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
    %% check known protocols and models etc?
    ok;
validate_event(out, Flags) ->
    Spec = output_spec(proplists:get_value(protocol,Flags)),
    hex:validate_flags(Flags, Spec).

output_spec(nexa) ->
    [{protocol,mandatory,{const,nexa},undefined},
     {unit,mandatory,{integer,$A,$P},$A},
     {channel,mandatory,{integer,1,16},1},
     {optional,dimmer,boolean,false}];
output_spec(nexax) ->
    [{protocol,mandatory,{const,nexax},undefined},
     {unit,mandatory,{integer,0,16#3fffffff},$A},
     {channel,mandatory,{integer,1,16},1}];
output_spec(waveman) ->
    [{protocol,mandatory,{const,waveman},undefined},
     {unit,mandatory,{integer,$A,$P},$A},
     {channel,mandatory,{integer,1,16},1}];
output_spec(sartano) ->
    [{protocol,mandatory,{const,sartano},undefined},
     {channel,mandatory,{integer,1,16#3ff},0}];
output_spec(ikea) ->
    [{protocol,mandatory,{const,ikea},undefined},
     {unit,mandatory,{integer,$A,$P},$A},
     {channel,mandatory,{integer,1,16},1},
     {dimmer,optional,boolean,false},
     {style,optional,{alt,[{const,smooth},{const,instant}]},smooth}];
output_spec(risingsun) ->
    [{protocol,mandatory,{const,risingsun},undefined},
     {unit,mandatory,{integer,1,4},1},
     {channel,mandatory,{integer,1,4},1}];
output_spec(_) ->
    [{protocol,mandatory,
      {alt,[{const,nexa},
	    {const,nexax},
	    {const,sartano},
	    {const,ikea},
	    {const,risingsun}]},undefined}].
