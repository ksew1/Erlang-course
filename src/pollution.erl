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
-export([test/0, create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3, get_maximum_gradient_station/2]).



create_monitor() -> #{}.

add_station(Name, Coords, Monitor) ->
  StationExists = fun({N, C}, _, Acc) -> Name =:= N orelse Coords =:= C orelse Acc end,

  case maps:fold(StationExists, false, Monitor) of
    false -> maps:put({Name, Coords}, #{}, Monitor);
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
    {ok, K, Data} ->
      case maps:is_key({Time, Type}, Data) of
        true -> {error, duplicate};
        false -> NewData = maps:put({Time, Type}, Value, Data),
          Monitor#{K => NewData}
      end;
    {error, station_not_found} ->
      {error, station_not_found}
  end.

remove_value(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, K, Data} ->
      case maps:is_key({Time, Type}, Data) of
        false -> {error, not_found};
        true -> NewData = maps:remove({Time, Type}, Data),
          Monitor#{K => NewData}
      end;
    {error, station_not_found} ->
      {error, station_not_found}
  end.

get_one_value(Key, Time, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, _, Data} ->
      case maps:is_key({Time, Type}, Data) of
        false -> {error, not_found};
        true -> maps:get({Time, Type}, Data)
      end;
    {error, station_not_found} ->
      {error, station_not_found}
  end.

get_station_mean(Key, Type, Monitor) ->
  case find_station(Key, Monitor) of
    {ok, _, Data} ->
      Mean = fun({_, T}, V, {Sum, Count}) when T =:= Type -> {V + Sum, Count + 1};
        (_, _, {Sum, Count}) -> {Sum, Count} end,
      {Total, NumSamples} = maps:fold(Mean, {0, 0}, Data),
      case NumSamples of
        0 -> {error, no_samples};
        _ -> Total / NumSamples
      end;
    {error, station_not_found} ->
      {error, station_not_found}
  end.


get_daily_mean({Day, _}, Type, Monitor) ->
  DailyMean = fun({{D, _}, T}, V, {Value, NumValues}) when T =:= Type andalso D =:= Day ->
    {Value + V, NumValues + 1};
    (_, _, Acc) -> Acc end,

  StationMean = fun(_, Data, {SumMeans, NumStationsWithValues}) ->
    {StationValue, NumValues} = maps:fold(DailyMean, {0, 0}, Data),
    case NumValues of
      0 -> {SumMeans, NumStationsWithValues};
      _ -> {SumMeans + StationValue / NumValues, NumStationsWithValues + 1}
    end
                end,
  {AllValue, AllNumStations} = maps:fold(StationMean, {0, 0}, Monitor),
  case AllNumStations of
    0 -> {error, no_stations};
    _ -> AllValue / AllNumStations
  end.


get_maximum_gradient_station(Type, Monitor) ->
  Gradient = fun({_, T1}, V1, {_, T2}, V2, Acc) when T1 =:= T2 andalso T1 =:= Type -> max(V2 - V1, Acc);
    (_, _, _, _, Acc) -> Acc end,

  StationGradient = fun(_, Data, {MaxStation, MaxGradient}) ->
    StationData = lists:sort(fun({{_, T1}, _}, {{_, T2}, _}) -> T1 < T2 end, maps:to_list(Data)),
    StationMaxGradient = lists:foldl(fun({K1, V1}, {K2, V2}) -> Gradient(K1, V1, K2, V2, V2 - V1) end, 0, StationData),
    case StationMaxGradient > MaxGradient of
      true -> {MaxStation, StationMaxGradient};
      false -> {MaxStation, MaxGradient}
    end
                    end,

  case maps:size(Monitor) of
    0 -> {error, no_stations};
    _ -> maps:fold(StationGradient, {none, infinity}, Monitor)
  end.


test() ->
  eunit:test([pollution_test]).
