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
	 event_spec/1,
	 init_event/2,
	 mod_event/2,
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
    case proplists:get_value(protocol,Flags) of
	{Protocol,PFlags} ->
	    output_protocol(Protocol, PFlags, Env);
	_ ->
	    lager:error("protocol format error")
    end.

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

mod_event(_Dir,_Flags) ->
    ok.

%%
%% validate_event(in | out, Flags::[{atom(),term()}])
%%
validate_event(in, _Flags) ->
    %% check known protocols and models etc?
    ok;
validate_event(out, Flags) ->
    hex:validate_flags(Flags, event_spec(out)).
    

event_spec(in) ->
    [];
event_spec(out) ->
    [{choice,protocol,
      [{'case',nexa,
	[{container,nexa,
	  [{leaf,unit,[{type,uint8,[{range,[{$A,$P}],[]}]},
		       {default,$A,[]}]},
	   {leaf,channel,[{type,uint8,[{range,[{1,16}],[]}]},
			  {default,1,[]}]},
	   {leaf,dimmer,[{type,boolean,[]},
			 {default,false,[]}]}
	  ]}
	]},

       {'case',nexax,
	[{container,nexax,
	  [{leaf,unit,[{type,uint32,[{range,[{0,16#3fffffff}],[]}]},
		       {default,0,[]}]},
	   {leaf,channel,[{type,uint8,[{range,[{1,16}],[]}]},
			  {default,1,[]}]}
	  ]}
	]},

       {'case',waveman,
	[{container,waveman,
	  [{leaf,unit,[{type,uint8,[{range,[{$A,$P}],[]}]},
		       {default,$A,[]}]},
	   {leaf,channel,[{type,uint8,[{range,[{1,16}],[]}]},
			  {default,1,[]}]}
	  ]}
	]},
       
       {'case',sartano,
	[{container,sartano,
	  [{leaf,channel,[{type,uint16,[{range,[{1,16#3ff}],[]}]},
			  {default,1,[]}]}
	  ]}
	]},

       {'case',ikea,
	[{container,ikea,
	  [{leaf,unit,[{type,uint8,[{range,[{1,16}],[]}]},
		       {default,1,[]}]},
	   {leaf,channel,[{type,uint8,[{range,[{1,10}],[]}]},
			  {default,1,[]}]},
	   {leaf,dimmer,[{type,boolean,[]},
			 {default,false,[]}]},
	   {leaf,style,[{type,enumeration,
			 [{enum,smooth,[]},
			  {enum,instant,[]}]},
			{default,smooth,[]}]}
	  ]}
	]},

       {'case',risingsun,
	[{container,risingsun,
	  [{leaf,unit,[{type,uint8,[{range,[{1,4}],[]}]},
		       {default,1,[]}]},
	   {leaf,channel,[{type,uint8,[{range,[{1,4}],[]}]},
			  {default,1,[]}]}
	  ]}
	]}
      ]}
    ].


