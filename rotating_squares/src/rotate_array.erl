-module(rotate_array).

-export([rotate/1
        ,make_square/1
        ]).

-include_lib("eunit/include/eunit.hrl").

make_square(Length) when Length rem 2 =:= 0 ->
    make_square(Length, fun() -> random:uniform(2)-1 end).

make_square(Length, ValueFun) ->
    {'square'
    ,Length
    ,[ [ValueFun() || _ <- lists:seq(1, Length)]
       || _ <- lists:seq(1, Length)
     ]
    }.

rotate(Square) ->
    multirec(fun is_len_one/1 %% indivisible
            ,fun identity/1 %% value
            ,fun divide_into_regions/1 %% divide
            ,fun rotate_and_combine/1 %% combine
            ,Square
            ).

multirec(Indivisable, Value, Divide, Combine, Data) ->
    case Indivisable(Data) of
        'true' -> Value(Data);
        'false' ->
            Parts = Divide(Data),
            MappedParts = lists:map(fun(Part) -> multirec(Indivisable, Value, Divide, Combine, Part) end, Parts),
            Combine(MappedParts)
    end.

%% rotate regions in pattern match
%% [UL UR LR LL] => [UR LR LL UL]
rotate_and_combine([UpperRight, LowerRight, LowerLeft, UpperLeft]) ->
    UpperHalf = square_zipwith(fun concat/2, UpperLeft, UpperRight),
    LowerHalf = square_zipwith(fun concat/2, LowerLeft, LowerRight),

    concat(UpperHalf, LowerHalf).

square_zipwith(ZipWith, {'square', Length, Left}, {'square', Length, Right}) ->
    {'square', Length, lists:zipwith(ZipWith, Left, Right)}.

concat({'square', Length, LeftRow}
      ,{'square', Length, RightRow}
      ) ->
    {'square', Length+Length, concat(LeftRow, RightRow)};
concat(LeftRow, RightRow) ->
    LeftRow ++ RightRow.

is_len_one({'square', 1, [_]}) -> 'true';
is_len_one(_) -> 'false'.

identity(X) -> X.

divide_into_regions(Square) ->
    UpperHalf = first_half(Square),
    LowerHalf = second_half(Square),

    [_UpperLeft = divide_half(UpperHalf, fun first_half/1)
    ,_UpperRight = divide_half(UpperHalf, fun second_half/1)
    ,_LowerRight = divide_half(LowerHalf, fun second_half/1)
    ,_LowerLeft = divide_half(LowerHalf, fun first_half/1)
    ].

divide_half({'rectangle', Length, Array}, HalvingFun) ->
    {'square', Length div 2, lists:map(HalvingFun, Array)}.

first_half({'square', Length, Array}) ->
    {'rectangle', Length, lists:sublist(Array, 1, Length div 2)};
first_half(Array) ->
    Length = length(Array),
    lists:sublist(Array, 1, Length div 2).

second_half({'square', Length, Array}) ->
    {'rectangle', Length, lists:sublist(Array, (Length div 2) + 1, Length div 2)};
second_half(Array) ->
    Length = length(Array),
    lists:sublist(Array, (Length div 2) + 1, Length div 2).

-ifdef(TEST).

rotate2_test() ->
    Square = {'square', 2, [[1,2],[3,4]]},
    Square90 = rotate(Square),
    ?assertEqual({'square', 2, [[3,1], [4,2]]}
                ,Square90
                ),
    Square180 = rotate(Square90),
    ?assertEqual({'square', 2, [[4,3],[2,1]]}
                ,Square180
                ),
    Square270 = rotate(Square180),
    ?assertEqual({'square', 2, [[2,4],[1,3]]}
                ,Square270
                ),
    Square360 = rotate(Square270),
    ?assertEqual(Square, Square360).

rotate4_test() ->
    Square = {'square', 4, make_test_square(16, 4)},
    Square90 = rotate(Square),
    ?assertEqual({'square', 4, [[13,9,5,1]
                               ,[14,10,6,2]
                               ,[15,11,7,3]
                               ,[16,12,8,4]
                               ]
                 }
                ,Square90
                ),
    Square180 = rotate(Square90),
    ?assertEqual({'square', 4, [[16,15,14,13]
                               ,[12,11,10,9]
                               ,[8,7,6,5]
                               ,[4,3,2,1]
                               ]
                 }
                ,Square180
                ),
    Square270 = rotate(Square180),
    ?assertEqual({'square', 4, [[4,8,12,16]
                               ,[3,7,11,15]
                               ,[2,6,10,14]
                               ,[1,5,9,13]
                               ]
                 }
                ,Square270
                ),
    Square360 = rotate(Square270),
    ?assertEqual(Square, Square360).

divide2_test() ->
    Square = {'square', 2, make_test_square(4, 2)},
    Regions = divide_into_regions(Square),

    ?assertEqual([{'square', 1, [[1]]} % UpperLeft
                 ,{'square', 1, [[2]]} % UpperRight
                 ,{'square', 1, [[4]]} % LowerRight
                 ,{'square', 1, [[3]]} % LowerLeft
                 ]
                ,Regions
                ),

    Square90 = {'square', 2, [[3,4],[2,1]]},
    Regions90 = divide_into_regions(Square90),
    ?assertEqual([{'square', 1, [[3]]}
                 ,{'square', 1, [[4]]}
                 ,{'square', 1, [[1]]}
                 ,{'square', 1, [[2]]}
                 ]
                ,Regions90
                ).

divide4_test() ->
    Square = {'square'
             ,4
             ,make_test_square(16, 4)
             },
    Regions = divide_into_regions(Square),
    ?assertEqual([{'square', 2, [[1,2],[5,6]]} % UpperLeft
                 ,{'square', 2, [[3,4],[7,8]]} % UpperRight
                 ,{'square', 2, [[11,12],[15,16]]} % LowerRight
                 ,{'square', 2, [[9,10],[13,14]]} % LowerLeft
                 ]
                ,Regions
                ).

make_test_square(N, Length) ->
    {Row, Square, _} = lists:foldl(fun make_test_square_row/2
                                  ,{[], [], Length}
                                  ,lists:seq(1, N)
                                  ),
    maybe_add_row(Row, Square).

maybe_add_row([], Square) ->
    lists:reverse(Square);
maybe_add_row(Row, Square) ->
    lists:reverse([lists:reverse(Row) | Square]).

make_test_square_row(I, {Row, Square, Length}) when I rem Length =:= 0 ->
    {[], [lists:reverse([I | Row]) | Square], Length};
make_test_square_row(I, {Row, Square, Length}) ->
    {[I | Row], Square, Length}.

-endif.
