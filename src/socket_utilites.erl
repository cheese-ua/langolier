%% Copyright
-module(socket_utilites).
-author("cheese").
-include("types.hrl").

%% API
-export([parseSocketList/1, parseSocket/1, get_name/1, get_name/2]).

%%---------------------------------------------------------
%%  Prepare list of #socket_info from application settings
%%---------------------------------------------------------
-spec(parseSocketList(_)-> [#socket_info{}]).
parseSocketList([]) ->
	[];
parseSocketList([Head | Tail]) ->
	[parseSocket(Head) | parseSocketList(Tail)].

%%---------------------------------------------------------
%%  Prepare #socket_info from application settings
%%---------------------------------------------------------
-spec(parseSocket(_)-> #socket_info{}).
parseSocket([{type, ServerType}, {ip, ServerIP}, {port, ServerPort}, {name, Name}]) ->
	#socket_info{ip = ServerIP, port =ServerPort, type = ServerType,name = Name	}.

%%---------------------------------------------------------
%%  get name param from application setting
%%---------------------------------------------------------
-spec( get_name([term()]) -> atom()).
get_name([]) ->
  unknown;
get_name([{Key, Val} | Tail]) ->
  case Key of
    name -> Val;
    _ -> get_name(Tail)
  end.

%%---------------------------------------------------------
%%  Prepare #socket_info from application settings
%%  and add prefix
%%---------------------------------------------------------
-spec( get_name([term()], [term()]) -> atom()).
get_name(List, Prefix) ->
  Name = get_name(List),
  Suffix = atom_to_list(Name),
  list_to_atom(Prefix ++ Suffix).
