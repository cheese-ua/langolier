%%%-------------------------------------------------------------------
%%% @author cheese
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2014 11:16 AM
%%%-------------------------------------------------------------------
-module(bytes_extension).
-author("cheese").

%% API
-export([bytes_to_hex/1]).

%% Interlan
-export([bytes_to_hex_inner/2]).

bytes_to_hex(Bytes) ->
  bytes_to_hex_inner(Bytes, []).



bytes_to_hex_inner(_Bytes, _Res) ->
  0.