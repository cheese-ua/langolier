%% Copyright
-module(main_socket_sup).
-author("cheese").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor
-export([init/1]).

%% API
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
	RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
	MaxRestarts = 0,
	MaxSecondsBetweenRestarts = 10,
	SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

	Restart = permanent, % permanent | transient | temporary
	Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

	SomeWorker = {main_socket,
		{main_socket, start_link, []},
		Restart, Shutdown, worker,
		[main_socket]},

	{ok, {SupFlags, [SomeWorker]}}.
