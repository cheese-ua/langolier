%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. окт 2013 9:03
%%%-------------------------------------------------------------------
-module(logger).
-author("cheese").

%% API
-export([info/2, info/3, error/2, error/3]).

info(Message, FileName) ->
	{{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
	Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, self()])),
  MessageDate = io_lib:fwrite("~s ~s",[Date , Message]),
  io:format("~s~n",[MessageDate]),
  file:write_file(FileName, MessageDate, [append]).

info(Format, Data, FileName) ->
	{{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
	Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, self()])),
	Message = io_lib:fwrite(Format, Data),
  MessageDate = io_lib:fwrite("~s ~s",[Date , Message]),
  io:format("~s",[MessageDate]),
  file:write_file(FileName, MessageDate, [append]).

error(Message, FileName) ->
  info(Message, FileName).

error(Format, Data, FileName) ->
  info(Format, Data, FileName).

