#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

-record(point, {index, top, left}).

main(_) ->
    Coordinates = coordinates(),
    P6_1 = p6_1(Coordinates),
    io:format("p6_1: ~p~n", [P6_1]).

p6_1(Cs) ->
    %% We know non-infinite points will be inside the bounded box
    %% so find the {top, left}, {bottom, right} coordinates
    {_TopLeft, _BottomRight}=Box = box_coordinates(Cs),
    %% filter coordinates for those inside the box since the others will extend outside the box
    BoundedPoints = lists:filter(fun(C) -> is_inside_box(C, Box) end, Cs),
    find_largest_area(Cs, Box, BoundedPoints).

find_largest_area(Cs, Box, BoundedCs) ->
    %% build a map of {Point, {WhoIndex, WhoPoint}}
    %% Point is a point in the box
    %% WhoPoint is the coordinate closests to Point
    %% WhoIndex is the index of the coordinate
    Points = build_points(Box),

    %% iterate through the box's points and find which coordinates are closest
    Marked = mark_points(Points, Cs),
    count_points(Marked, BoundedCs).

count_points(Marked, BoundedCs) ->
    {Area, _} = lists:foldl(fun count_point/2, {0, Marked}, BoundedCs),
    Area.

count_point(#point{index=I}, {Area, Marked}) ->
    {max(area(I, Marked), Area), Marked}.

area(Index, Marked) ->
    {Area, _} = maps:fold(fun count_index_points/3, {0, Index}, Marked),
    Area.

count_index_points(_Point, {Index, _}, {Area, Index}) ->
    {Area+1, Index};
count_index_points(_Point, {I, _}, {Area, Index}) when I =:= Index+32 ->
    {Area+1, Index};
count_index_points(_, _, Acc) ->
    Acc.

mark_points(Points, Cs) ->
    lists:foldl(fun mark_point/2, Points, Cs).

mark_point(Coordinate, Points) ->
    {Coordinate, Marked} = maps:fold(fun mark_point_distances/3, {Coordinate, Points}, Points),
    %% print_box(Marked),
    Marked.

mark_point_distances(#point{top=Top, left=Left}=Point
                    ,_Who
                    ,{#point{top=Top, left=Left, index=Index}=C, Points}
                    ) ->
    {C, maps:put(Point, {Index, 0}, Points)};
mark_point_distances(BoxPoint
                    ,{'undefined', _}
                    ,{#point{index=Index}=Coordinate, Points}
                    ) ->
    Distance = manhattan_distance(BoxPoint, Coordinate),
    {Coordinate, maps:put(BoxPoint, {Index+32, Distance}, Points)};
mark_point_distances(BoxPoint
                    ,{_WhoIndex, WhoDistance}
                    ,{Coordinate, BoxPoints}
                    ) ->
    case manhattan_distance(BoxPoint, Coordinate) of
        CDistance when CDistance < WhoDistance ->
            {Coordinate, maps:put(BoxPoint, {Coordinate#point.index+32, CDistance}, BoxPoints)};
        WhoDistance ->
            {Coordinate, maps:put(BoxPoint, {$., WhoDistance}, BoxPoints)};
        _CDist ->
            {Coordinate, BoxPoints}
    end.

build_points({#point{top=Top, left=Left}
             ,#point{top=Bottom, left=Right}
             }) ->
    maps:from_list([{#point{top=Y, left=X}
                    ,{'undefined', 0} %% {Who, ManhattanDistance}
                    } || X <- lists:seq(Left, Right),
                        Y <- lists:seq(Top, Bottom)
                   ]).

is_inside_box(#point{left=X, top=Y}, {#point{top=Top, left=Left}, #point{top=Bottom, left=Right}}) ->
    X > Left andalso X < Right
        andalso Y > Top andalso Y < Bottom.

box_coordinates([#point{left=X, top=Y} | Coordinates]) ->
    box_coordinates(Coordinates, {#point{left=X, top=Y}, #point{left=X, top=Y}}).

box_coordinates([], Box) -> Box;
box_coordinates([#point{left=X, top=Y} | Coordinates]
               ,{#point{top=Top, left=Left}
                ,#point{top=Bottom, left=Right}
                }
               ) ->
    box_coordinates(Coordinates, {#point{top=top(Top, Y), left=left(Left, X)}
                                 ,#point{top=bottom(Bottom, Y), left=right(Right, X)}
                                 }).

top(Top, Y) when Top < Y -> Top;
top(_Top, Y) -> Y.

left(Left, X) when Left < X -> Left;
left(_Left, X) -> X.

bottom(Bottom, Y) when Bottom > Y -> Bottom;
bottom(_Bottom, Y) -> Y.

right(Right, X) when Right > X -> Right;
right(_Right, X) -> X.

coordinates() ->
    %% Bin = <<"1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n">>,
    {'ok', Bin} = file:read_file("p6.txt"),
    coordinates(binary:split(Bin, <<"\n">>, ['global'])).

coordinates(Bins) ->
    coordinates(Bins, $A, []).

coordinates([], _, Cs) -> lists:reverse(Cs);
coordinates([<<>>|Bins], Index, Cs) ->
    coordinates(Bins, Index, Cs);
coordinates([Bin|Bins], Index, Cs) ->
    {'match', [Left, Top]} = re:run(Bin, <<"(\\d+),\\s*(\\d+)">>, [{'capture', 'all_but_first', 'binary'}]),
    coordinates(Bins, Index+1, [#point{left=binary_to_integer(Left)
                                      ,top=binary_to_integer(Top)
                                      ,index=Index
                                      }
                                | Cs
                               ]).

manhattan_distance(#point{top=PTop, left=PLeft}, #point{top=QTop, left=QLeft}) ->
    abs(PTop-QTop) + abs(PLeft-QLeft).

print_box(BoxPoints) ->
    [{#point{top=Y}, {W, _}} | Points] = lists:keysort(1, maps:to_list(BoxPoints)),
    io:format("~s", [[W]]),
    lists:foldl(fun print_point/2, Y, Points),
    io:format("~n~n").

print_point({#point{top=Y}, {'undefined', _}}, Y) ->
    io:format("@"),
    Y;
print_point({#point{top=Y}, {'undefined', _}}, _Y) ->
    io:format("~n@"),
    Y;
print_point({#point{top=Y}, {Who, _}}, Y) ->
    io:format("~s", [[Who]]),
    Y;
print_point({#point{top=Y}, {Who, _}}, _) ->
    io:format("~n~s", [[Who]]),
    Y.
