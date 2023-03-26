%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. mar 2023 11:53
%%%-------------------------------------------------------------------
-module(myList).
-author("Karol").

%% API
-export([tailSumFloats/2, power/2, contains/2, duplicateElements/1, sumFloats/1, f/0]).

power(_, 0) -> 1;
power(X, N) -> X * power(X, N - 1).

contains([], _) -> false;
contains([H | _], H) -> true;
contains([_ | T], X) -> contains(T, X).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H | duplicateElements(T)].

sumFloats([]) -> 0;
sumFloats([H | T]) -> H + sumFloats(T).

tailSumFloats([], Acc) -> Acc;
tailSumFloats([H | T], Acc) -> tailSumFloats(T, H + Acc).

f() -> [{X, X*2} || X <- lists:seq(1, 10)].



