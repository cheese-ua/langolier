-module(langolier_sup).

-behaviour(supervisor).

-export([start_link/2, init/1]).

start_link(MainSocket, ClientsSockets) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket, ClientsSockets]).


init([MainSocket, ClientsSockets]) ->
		logger:info("Start supervisor: ~w~n", [?MODULE]),
		RestartStrategy = one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = brutal_kill,

    SomeWorker = {top_handler,
		  {top_handler, start, [MainSocket, ClientsSockets]},
		  Restart, Shutdown, worker, 
		  [top_handler]},

    {ok, {SupFlags, [SomeWorker]}}.

