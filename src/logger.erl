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
-export([error/1, error/2, info/1, info/2]).

info(Message) ->
	{{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
	Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, self()])),
	io:format("~s ~s~n",[Date , Message]).

info(Format, Data) ->
	{{Year, Month, Day}, {Hour, Min, Second} } = calendar:local_time(),
	Date = lists:flatten(io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ~w ", [Year, Month, Day, Hour, Min, Second, self()])),
	Message = io_lib:format(Format, Data),
	io:format("~s ~s",[Date , Message]).

error(Message) ->
	info(Message).

error(Format, Data) ->
	info(Format, Data).
