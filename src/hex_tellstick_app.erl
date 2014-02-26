-module(hex_tellstick_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    hex_tellstick_sup:start_link().

stop(_State) ->
    ok.
