%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2023 22:21
%%%-------------------------------------------------------------------
-module(onp).
-author("Karol").

%% API
-export([onp/1, test/0]).

onp(Expr) -> convert_to_onp(string:tokens(Expr, " "), []).
test() -> {
  onp("1 2 3 * + 4 5 / - 6 +") =:= 12.2,
  onp("1 2 + 3 + 4 + 5 + 6 7 * +") =:= 57,
  onp("4 7 + 3 / 2 19 - *") =:= -62.33333333333333,
  onp("2 3 pow") =:= 8.0
  }.
% if there is only one element in the stack, it is the result
convert_to_onp([], [X]) -> X;
convert_to_onp([H|T], [X |Stack]) when H =:= "sqrt" -> convert_to_onp(T, [math:sqrt(X)|Stack]);
convert_to_onp([H|T], [X |Stack]) when H =:= "sin" -> convert_to_onp(T, [math:sin(X)|Stack]);
convert_to_onp([H|T], [X |Stack]) when H =:= "cos" -> convert_to_onp(T, [math:cos(X)|Stack]);
convert_to_onp([H|T], [X |Stack]) when H =:= "tan" -> convert_to_onp(T, [math:tan(X)|Stack]);
convert_to_onp([H|T], [X, Y|Stack]) when H =:= "+" -> convert_to_onp(T, [Y + X|Stack]);
convert_to_onp([H|T], [X, Y|Stack]) when H =:= "-" -> convert_to_onp(T, [Y - X |Stack]);
convert_to_onp([H|T], [X, Y|Stack]) when H =:= "*" -> convert_to_onp(T, [Y * X|Stack]);
convert_to_onp([H|T], [X, Y|Stack]) when H =:= "/" -> convert_to_onp(T, [Y / X|Stack]);
convert_to_onp([H|T], [X, Y|Stack]) when H =:= "pow" -> convert_to_onp(T, [math:pow(Y, X)|Stack]);
convert_to_onp([H|T], Stack) -> {Int, _} = string:to_integer(H), convert_to_onp(T, [Int |Stack]).
%convert_to_onp([H|T], Stack) -> convert_to_onp(T, [erlang:list_to_float(H)|Stack]).



%convert_to_onp([H|T], [X, Y|Stack]) ->
%  case H of
%    "+" -> convert_to_onp(T, [X + Y|Stack]);
%    "-" -> convert_to_onp(T, [X - Y|Stack]);
%    "*" -> convert_to_onp(T, [X * Y|Stack]);
%    "/" -> convert_to_onp(T, [X / Y|Stack])
%  end;
%convert_to_onp([H|T], Stack) -> convert_to_onp(T, [erlang:list_to_integer(H)|Stack]).








