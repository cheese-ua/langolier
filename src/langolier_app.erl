-module(langolier_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(langolier),
    ok.


start(_StartType, _StartArgs) ->
		MainSocket = application:get_env(langolier, main_socket),
		ClientsSockets = application:get_env(langolier, clients_sockets),
    langolier_sup:start_link(MainSocket, ClientsSockets).

stop(_State) ->
    ok.
