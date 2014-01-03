-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, ClientsSockets]) ->
		logger:info("Start supervisor: ~w~n", [?MODULE]),
		RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 2,

    Restart = permanent,
    Shutdown = 2000,

    SomeWorker = {top_handler,
		  {top_handler, start, [MainSocket, ClientsSockets]},
		  Restart, Shutdown, worker, 
		  []},

    {ok, {
      {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
      [SomeWorker]
         }
    }.

