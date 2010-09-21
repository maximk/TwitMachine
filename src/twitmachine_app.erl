%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the twitmachine application.

-module(twitmachine_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for twitmachine.
start(_Type, _StartArgs) ->
    twitmachine_deps:ensure(),
    twitmachine_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for twitmachine.
stop(_State) ->
    ok.
