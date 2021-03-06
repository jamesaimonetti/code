-module(fly6).

-export([grab_clip/1
        ,clip_seconds/0
        ]).

-include("cycliq.hrl").

clip_seconds() -> 600. %% 10 minutes

grab_clip(Filename) ->
    clip_metadata(Filename).

clip_metadata(Filename) ->
    [H1, H2
    ,M1, M2
     | Index
    ]= filename:basename(Filename, ".AVI"),

    [_, _, _
    ,Y
    ,Mo1, Mo2
    ,D1, D2
    ] = filename:basename(filename:dirname(Filename)),

    #clip{orig_path=Filename
         ,year=year(Y-$0)
         ,month=month([Mo1, Mo2])
         ,day=day([D1, D2])
         ,hour=hour([H1, H2])
         ,minute=minute([M1, M2])
         ,index=index(Index)
         ,module=?MODULE
         }.

year(N) ->
    {Y, _, _} = date(),
    Y - ((Y rem 10) - N).

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
