%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 17:06
%%%-------------------------------------------------------------------
-module(qsort).
-author("Karol").

%% API
-export([qs/1, random_elems/3, compare_speeds/3, f1/1, f2/1, f3/1]).

less_than(List, Arg) -> [X || X <- List, X < Arg].
grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) -> [rand:uniform(Max) + Min - 1 || _ <- lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Fun1: ~pms~nFun2: ~pms~n", [Time1, Time2]).

f1(L) -> lists:map(fun($a) -> $e; ($e) -> $o; (X) -> X end, L).

f2(L) -> lists:foldl(fun(X, Y) when X rem 3 == 0 -> Y + 1; (_, Y) -> Y end, 0, L).

f3(L) -> lists:foldl(fun(X, Y)  -> Y + X end, 0, L).

