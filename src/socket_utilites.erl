%% Copyright
-module(socket_utilites).
-author("cheese").
-include("types.hrl").

%% API
-export([parseSocketList/1, parseSocket/1, get_name/1, get_name/2]).

parseSocketList([]) ->
	[];
parseSocketList([Head | Tail]) ->
	[parseSocket(Head) | parseSocketList(Tail)].

parseSocket([{type, ServerType}, {ip, ServerIP}, {port, ServerPort}, {name, Name}]) ->
	#socket_info{ip = ServerIP, port =ServerPort, type = ServerType,name = Name	}.

-spec( get_name([term()]) -> atom()).
get_name([]) ->
  unknown;
get_name([{Key, Val} | Tail]) ->
  case Key of
    name -> Val;
    _ -> get_name(Tail)
  end.


get_name(List, Prefix) ->
  Name = get_name(List),
  Suffix = atom_to_list(Name),
  list_to_atom(Prefix ++ Suffix).
