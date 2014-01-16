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
-export([bin_to_hexstr/1, list_to_hexstr/1]).

-export([hex/1, int_to_hex/1]).

hex(N) when N == 0 ->
  $0;
hex(N) when N == 1 ->
  $1;
hex(N) when N == 2 ->
  $2;
hex(N) when N == 3 ->
  $3;
hex(N) when N == 4 ->
  $4;
hex(N) when N == 5 ->
  $5;
hex(N) when N == 6 ->
  $6;
hex(N) when N == 7 ->
  $7;
hex(N) when N == 8 ->
  $8;
hex(N) when N == 9 ->
  $9;
hex(N) when N == 10 ->
  $A;
hex(N) when N == 11 ->
  $B;
hex(N) when N == 12 ->
  $C;
hex(N) when N == 13 ->
  $D;
hex(N) when N == 14 ->
  $E;
hex(N) when N == 15 ->
  $F;
hex(_N)->
  $?.

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

list_to_hexstr([]) ->
  [];
list_to_hexstr([H|T]) ->
  int_to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bytes) ->
  list_to_bitstring(list_to_hexstr(binary_to_list(Bytes))).

