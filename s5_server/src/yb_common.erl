-module(yb_common).
-compile(export_all).

sleep(T) ->
  receive
  after T ->
      true
  end.

head(L) ->
  [H|_T] = L,
  H.

trim_head(L, H) ->
  case L of 
    [H|T] ->
      trim_head(T, H);
    L1 ->
      L1
  end.

bin_encode(Bin) ->
  bin_encode(Bin, []).

bin_encode(<<>>, L) ->
  list_to_binary(lists:reverse(L));
bin_encode(<<H:8/integer, T/binary>>, L) -> 
  bin_encode(T, [ 255 - H |  L]).


bin_decode(Bin) ->
  bin_decode(Bin, []).

bin_decode(<<>>, L) ->
  list_to_binary(lists:reverse(L));
bin_decode(<<H:8/integer, T/binary>>, L) ->
  bin_decode(T, [255 - H | L]).

%
print(_Format, _L) ->
  io:format(_Format, _L).

%qing green color
print_info(Format, L) ->
  io:format("[~p]\e[1;36m" ++ Format ++ "\e[0m~n", [self()|L]).

%red color
print_error(Format, L) ->
  io:format("\e[1;31m" ++ Format ++ "\e[0m~n", L).
