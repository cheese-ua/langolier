%% Copyright
-module(main_socket_sup).
-author("cheese").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor
-export([init/1]).

%% API
start_link(MainSocket) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket]).

%% supervisor callbacks
init([SocketParam]) ->
  logger:info("Start supervisor: ~w~n", [?MODULE]),
	RestartStrategy = one_for_one,
	MaxRestarts = 2,
	MaxSecondsBetweenRestarts = 5000,

	RestartType = permanent, % permanent | transient | temporary
	Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

	Child = {main_socket,
		{main_socket, start_link, [socket_utilites:parseSocket(SocketParam)]},
		RestartType, Shutdown, worker,
		[]},

	{ok, {
        {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
        [Child]
      }
  }.
