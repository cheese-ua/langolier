-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).
-define(LOG_FILE, "log/app.log").

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, ClientsSockets]) ->
		logger:info("Start supervisor: ~w~n", [?MODULE], ?LOG_FILE),

    MainSocketWorker = {main_socket_sup,
		  {main_socket_sup, start_link, [MainSocket]},
      permanent, 2000, supervisor,
		  []},

    ConsumerSockets = socket_utilites:parseSocketList(ClientsSockets),

    ConsumerSocketWorker = {super_consumer,
      {consumer_socket_sup, start_link, [ConsumerSockets]},
      permanent, 2000, supervisor,
      []},

    {ok, {
      {one_for_one, 2, 5},
      [MainSocketWorker, ConsumerSocketWorker]
         }
    }.
