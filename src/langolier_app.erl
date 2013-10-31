-module(langolier_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(langolier),
    ok.


start(_StartType, _StartArgs) ->
    langolier_sup:start_link().

    
stop(_State) ->
    ok.
