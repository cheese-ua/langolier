-module(langolier_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).
-define(LOG_FILE, "log/app.log").

%%consumer_socket, consumer_socket_sup, main_socket_sup, socket_utilites, top_handler
start() ->
    application:start(langolier_app),
    ok.

start(_StartType, _StartArgs) ->
    logger:register_file(?LOG_FILE),
		logger:info("Start application: ~w~n", [?MODULE], ?LOG_FILE),

    {ok, MainSocket} = application:get_env(langolier_app, main_socket),
    logger:info("MainSocket: ~p~n", [MainSocket], ?LOG_FILE),

    {ok, ClientsSockets} = application:get_env(langolier_app, clients_sockets),
    logger:info("ClientsSockets: ~p~n", [ClientsSockets], ?LOG_FILE),

    langolier_sup:start_link(MainSocket, ClientsSockets).

stop(_State) ->
  logger:info("Stop application: ~w~n", [?MODULE], ?LOG_FILE),
    ok.

