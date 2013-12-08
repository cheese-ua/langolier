%% Copyright
-module(top_handler).
-author("cheese").

-include("types.hrl").

%% API
-export([start/2]).

start(MainSocket, _ClientsSockets) ->
	logger:info("init: ~w~n", [?MODULE]),
	PidMain = main_socket_sup:start_link(socket_utilites:parseSocket(MainSocket)),
	{ok, {PidMain}}.

