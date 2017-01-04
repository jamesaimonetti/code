-module(rotate_quadtree).

-export([rotate/1]).

-record(square, {ul, ur
                ,ll, lr
                }).

rotate(Square) ->
    generics:multirec(fun is_leaf/1 %% indivisible
                     ,fun identity/1 %% value
                     ,fun divide_into_regions/1 %% divide
                     ,fun rotate_and_combine/1 %% combine
                     ,Square
                     ).

is_leaf(#square{}) -> 'false';
is_leaf(_) -> 'true'.

identity(X) -> X.

divide_into_regions(#square{ul=UpperLeft, ur=UpperRight
                           ,ll=LowerLeft, lr=LowerRight
                           }
                   ) ->
    [UpperLeft, UpperRight, LowerRight, LowerLeft].

rotate_and_combine([UpperLeft, UpperRight, LowerRight, LowerLeft]=_Regions) ->
    #square{ul=LowerLeft,  ur=UpperLeft
           ,ll=LowerRight, lr=UpperRight
           }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

rotate2_test() ->
    Square = #square{ul=1, ur=2
                    ,ll=3, lr=4
                    },

    Square90 = rotate(Square),

    dbg:stop_clear(),
    dbg:stop(),

    ?assertEqual(#square{ul=3, ur=1
                        ,ll=4, lr=2
                        }
                ,Square90
                ),
    Square180 = rotate(Square90),
    ?assertEqual(#square{ul=4, ur=3
                        ,ll=2, lr=1
                        }
                ,Square180
                ),
    Square270 = rotate(Square180),
    ?assertEqual(#square{ul=2, ur=4
                        ,ll=1, lr=3
                        }
                ,Square270
                ),
    Square360 = rotate(Square270),
    ?assertEqual(Square, Square360).

-endif.
