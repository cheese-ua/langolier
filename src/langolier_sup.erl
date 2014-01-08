-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, _ClientsSockets]) ->
		logger:info("Start supervisor: ~w~n", [?MODULE]),
		RestartStrategy = one_for_one,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2000,

    Restart = permanent,
    Shutdown = 2000,

    SomeWorker = {main_socket_sup,
		  {main_socket_sup, start_link, [MainSocket]},
		  Restart, Shutdown, supervisor,
		  []},

    {ok, {
      {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
      [SomeWorker]
         }
    }.

