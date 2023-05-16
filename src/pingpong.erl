%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2023 16:57
%%%-------------------------------------------------------------------
-module(pingpong).
-author("Karol").

%% API
-export([start/0, stop/0, play/1]).

-define(TIMEOUT, 20000).

start() ->
  Ping = spawn(fun() -> ping_loop(0) end),
  Pong = spawn(fun() -> pong_loop() end),
  register(ping, Ping),
  register(pong, Pong).

stop() ->
  ping ! stop,
  pong ! stop.

play(N) when is_integer(N) ->
  ping ! N.

ping_loop(Sum) ->
  timer:sleep(1000),
  receive
    0 ->
      io:format("Ping: Reached 0~n"),
      ping_loop(Sum);
    stop ->
      io:format("Ping: Stopping~n");
    N when N > 0 ->
      NewSum = Sum + N,
      io:format("Ping: Received ball with ~p remaining ~n", [N]),
      pong ! N - 1,
      ping_loop(NewSum);
    _ ->
      ping_loop(Sum)
    after ?TIMEOUT ->
      io:format("Ping: Timeout~n")
  end.

pong_loop() ->
  timer:sleep(1000),
  receive
    0 ->
      io:format("Pong: Reached 0~n"),
      pong_loop();
    stop ->
      io:format("Pong: Stopping~n");
    N when N > 0 ->
      io:format("Pong: Received ball with ~p remaining ~n", [N]),
      ping ! N - 1,
      pong_loop();
    _ ->
      pong_loop()
    after ?TIMEOUT ->
      io:format("Pong: Timeout~n")
  end.



