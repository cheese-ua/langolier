-module(langolier_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).

%%consumer_socket, consumer_socket_sup, main_socket_sup, socket_utilites, top_handler
start() ->
    application:start(langolier_app),
    ok.

start(_StartType, _StartArgs) ->
		logger:info("Start application: ~w~n", [?MODULE]),
		{ok, MainSocket} = application:get_env(langolier_app, main_socket),
		{ok, ClientsSockets} = application:get_env(langolier_app, clients_sockets),
    langolier_sup:start_link(MainSocket, ClientsSockets).

stop(_State) ->
    ok.
