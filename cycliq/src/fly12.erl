-module(fly12).

-export([grab_clip/1]).

-include("cycliq.hrl").

grab_clip(Filename) ->
    clip_metadata(Filename).

clip_metadata(Filename) ->
    [Mo1, Mo2
    ,D1, D2
    ,H1, H2
    ,M1, M2
    ,$_
     | Index
    ]= filename:basename(Filename, ".MP4"),

    [_, _, _, $_ | Year] = filename:basename(filename:dirname(Filename)),

    #clip{orig_path=Filename
         ,year=year(Year)
         ,month=month([Mo1, Mo2])
         ,day=day([D1, D2])
         ,hour=hour([H1, H2])
         ,minute=minute([M1, M2])
         ,index=index(Index)
         }.

year([_, _, _, _] = YearList) ->
    list_to_integer(YearList).

month([_, _] = MonthList) ->
    list_to_integer(MonthList).

day([_, _] = DayList) ->
    list_to_integer(DayList).

hour([_, _] = HourList) ->
    list_to_integer(HourList).

minute([_, _] = MinuteList) ->
    list_to_integer(MinuteList).

index([_, _, _, _] = IndexList) ->
    list_to_integer(IndexList).
