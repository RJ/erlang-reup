-module(reup_app).
-behaviour(application).

-export([start/0]).
-export([start/2]).
-export([stop/1]).

start() ->
    application:start(reup).

start(_Type, _Args) ->
    reup_sup:start_link().

stop(_State) ->
    ok.
