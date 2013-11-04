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
	io_lib:format("~w~n",[Message]).
  %%error_logger:info_msg(Message).

info(Format, Data) ->
	io_lib:format(Format, [Data]).
  %%error_logger:info_msg(Format, Data).

error(Message) ->
%%error_logger:error_msg(Message).
	io_lib:format("~w~n",[Message]).

error(Format, Data) ->
%%error_logger:error_msg(Format, Data).
	io_lib:format(Format, [Data]).
