-module(langolier_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).
-include("logger.hrl").

start() ->
    application:start(langolier),
    ok.


start(_StartType, _StartArgs) ->
    langolier_sup:start_link().

    
stop(_State) ->
    ok.
