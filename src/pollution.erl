%%%-------------------------------------------------------------------
%%% @author Karol
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar 2023 18:04
%%%-------------------------------------------------------------------
-module(pollution).
-author("Karol").

%% API
-export([test/0, create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4]).

-record(station, {name, coords, data}).
-record(reading, {time, value}).

create_monitor() -> #{}.

add_station(Name, Coords, Monitor) ->
  StationExists = fun({N, C}, _, Acc) -> Name =:= N orelse Coords =:= C orelse Acc end,

  case maps:fold(StationExists, false, Monitor) of
    false -> Monitor#{{Name, Coords} => #station{name = Name, coords = Coords, data = #{}}};
    true -> {error, duplicate}
  end.

find_station(Key, Monitor) ->
  FindStation = fun
                  ({Name, Coords}, Value, _) when Key =:= Name orelse Key =:= Coords -> {{Name, Coords}, Value};
                  (_, _, Acc) -> Acc end,
  case maps:fold(FindStation, none, Monitor) of
    {K, V} -> {ok, K, V};
    none -> {error, station_not_found}
  end.

add_value(Key, Time, Type, Value, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, K, Station} ->
      NewData = maps:put(Type, #reading{time = Time, value = Value}, Station#station.data),
      Monitor#{K => Station#station{data = NewData}};
    {error, station_not_found} ->
      {error, station_not_found}
  end.

remove_value(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, K, Station} ->
      NewData = maps:remove({Time, Type}, Station#station.data),
      Monitor#{K => Station#station{data = NewData}};
    {error, station_not_found} ->
      {error, station_not_found}
  end.

get_one_value(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, _, Station} ->
      maps:get({Time, Type}, Station#station.data);
    {error, station_not_found} ->
      {error, station_not_found}
  end.

test() ->
  P = pollution:create_monitor(),
  P1 = pollution:add_station("A", {50.2345, 18.3445}, P),
  P2 = pollution:add_value({50.2345, 18.3445}, calendar:local_time(), "PM10", 59, P1),
  P3 = pollution:add_value("A", calendar:local_time(), "PM2,5", 113, P2),
  P4 = pollution:remove_value("A", calendar:local_time(), "PM10", P3),
  io:format("~p ~n ~p", [P4, pollution:get_one_value("A", calendar:local_time(), "PM2,5", P4)]).

