%% Copyright
-module(main_socket_sup).
-author("cheese").

-include("types.hrl").
-behaviour(supervisor).

%% API
-export([start_link/1, handler/1]).

%% supervisor
-export([init/1]).

%% API
start_link(MainSocket) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [MainSocket]).

%% supervisor callbacks
init([SocketParam]) ->
  logger:info("Start supervisor: ~w~n", [?MODULE]),

  InitSocketParam = socket_utilites:parseSocket(SocketParam),

	InstanceSocket = {main_socket,
		{main_socket, start_link, [InitSocketParam, fun main_socket_sup:handler/1]},
    permanent, 2, worker,
		[]},

	{ok, {
        {one_for_one,2,5},
        [InstanceSocket]
      }
  }.

handler(Bytes) ->
  logger:info("Handle message ~w~n", [Bytes]).
