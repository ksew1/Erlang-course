%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. kwi 2023 17:38
%%%-------------------------------------------------------------------
-module(locker).
-author("Karol").

%% API
-export([main/0, find_min_distance/2, find_min_distance/3, start_processes/2]).

rand_pos() ->
  rand:uniform(10000).

generate_locations(N) ->
  [{rand_pos(), rand_pos()} || _ <- lists:seq(1, N)].

locker_locations() ->
  generate_locations(10).

people_locations() ->
  generate_locations(20).

dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

find_min_distance(PeopleLocations, LockerLocations) ->
  Distances = [{dist(Person, Locker), {Person, Locker}}
    || Person <- PeopleLocations, Locker <- LockerLocations],
  lists:min(Distances).

find_min_distance(PeopleLocations, LockerLocations, ParentPID) ->
  Result = find_min_distance(PeopleLocations, LockerLocations),
  ParentPID ! Result.

start_processes(PeopleLocations, LockerLocations) ->
  [spawn(fun() -> find_min_distance([Person], LockerLocations, self()) end)
    || Person <- PeopleLocations],

  Distances = [receive M -> M end || _ <- PeopleLocations],
  lists:min(Distances).







main() ->
  PeopleLocations = people_locations(),
  LockerLocations = locker_locations(),
  StartTime = os:timestamp(),
  ResultSequential = find_min_distance(PeopleLocations, LockerLocations),
  EndTime = os:timestamp(),
  DurationSequential = timer:now_diff(EndTime, StartTime),

  StartTimeParallel = os:timestamp(),
  MinResultParallel = start_processes(PeopleLocations, LockerLocations),
  EndTimeParallel = os:timestamp(),
  DurationParallel = timer:now_diff(EndTimeParallel, StartTimeParallel),

  io:format("Sequential result: ~p~n", [ResultSequential]),
  io:format("Sequential duration: ~p microseconds~n", [DurationSequential]),
  io:format("Parallel result: ~p~n", [MinResultParallel]),
  io:format("Parallel duration: ~p microseconds~n", [DurationParallel]).



