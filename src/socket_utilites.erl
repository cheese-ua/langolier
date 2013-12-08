%% Copyright
-module(socket_utilites).
-author("cheese").
-include("types.hrl").

%% API
-export([parseSocketList/1, parseSocket/1]).

parseSocketList([]) ->
	[];
parseSocketList([Head | Tail]) ->
	[parseSocket(Head) | parseSocketList(Tail)].

parseSocket([{type, ServerType}, {ip, ServerIP}, {port, ServerPort}]) ->
	#socket_info{
			ip = ServerIP,
			port =ServerPort,
			type = ServerType
	}.
