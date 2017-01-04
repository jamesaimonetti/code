-module(rotate_quadtree).

-export([rotate/1
        ,to_quad/1
        ,to_square/1
        ]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(quad, {ul, ur
              ,ll, lr
              }).

-type quad_recursive() :: #quad{ul::#quad{}, ur::#quad{}
                               ,ll::#quad{}, lr::#quad{}
                               }.
-type quad_base() :: #quad{ul::integer(), ur::integer()
                          ,ll::integer(), lr::integer()
                          }.

-type quad() :: quad_recursive() | quad_base().

rotate(Square) ->
    generics:multirec(fun is_leaf/1 %% indivisible
                     ,fun identity/1 %% value
                     ,fun divide_into_regions/1 %% divide
                     ,fun rotate_and_combine/1 %% combine
                     ,Square
                     ).

-spec is_leaf(quad()) -> boolean().
is_leaf(#quad{}) -> 'false';
is_leaf(_) -> 'true'.

-spec identity(X) -> X.
identity(X) -> X.

divide_into_regions(#quad{ul=UpperLeft, ur=UpperRight
                         ,ll=LowerLeft, lr=LowerRight
                         }
                   ) ->
    [UpperLeft, UpperRight, LowerRight, LowerLeft].

rotate_and_combine([UpperLeft, UpperRight, LowerRight, LowerLeft]=_Regions) ->
    #quad{ul=LowerLeft,  ur=UpperLeft
         ,ll=LowerRight, lr=UpperRight
         }.

to_quad(Square) ->
    generics:multirec(fun is_one_by_one/1
                     ,fun scalar/1
                     ,fun rotate_array:divide_into_regions/1
                     ,fun regions_to_quad_tree/1
                     ,Square
                     ).

is_one_by_one({'square', 1, [[_]]}) -> 'true';
is_one_by_one(_) -> 'false'.

scalar({'square', 1, [[Scalar]]}) -> Scalar.

regions_to_quad_tree([UpperLeft, UpperRight, LowerRight, LowerLeft]) ->
    #quad{ul=UpperLeft, ur=UpperRight
         ,ll=LowerLeft, lr=LowerRight
         }.

to_square(#quad{}=Quad) ->
    generics:multirec(fun is_smallest_square/1
                     ,fun to_2d/1
                     ,fun quad_regions/1
                     ,fun combine_regions/1
                     ,Quad
                     ).

is_smallest_square(#quad{ul=#quad{}}) -> 'false';
is_smallest_square(#quad{}) -> 'true'.

to_2d(#quad{ul=UpperLeft, ur=UpperRight
           ,ll=LowerLeft, lr=LowerRight
           }
     ) ->
    {'square', 2, [[UpperLeft, UpperRight]
                  ,[LowerLeft, LowerRight]
                  ]
    }.

quad_regions(#quad{ul=UpperLeft, ur=UpperRight
                  ,ll=LowerLeft, lr=LowerRight
                  }
            ) ->
    [UpperLeft, UpperRight, LowerRight, LowerLeft].

combine_regions(Regions) ->
    rotate_array:combine(Regions).

-ifdef(TEST).

rotate2_test() ->
    Square = #quad{ul=1, ur=2
                  ,ll=3, lr=4
                  },

    Square90 = rotate(Square),

    dbg:stop_clear(),
    dbg:stop(),

    ?assertEqual(#quad{ul=3, ur=1
                      ,ll=4, lr=2
                      }
                ,Square90
                ),
    Square180 = rotate(Square90),
    ?assertEqual(#quad{ul=4, ur=3
                      ,ll=2, lr=1
                      }
                ,Square180
                ),
    Square270 = rotate(Square180),
    ?assertEqual(#quad{ul=2, ur=4
                      ,ll=1, lr=3
                      }
                ,Square270
                ),
    Square360 = rotate(Square270),
    ?assertEqual(Square, Square360).

to_quad2_test() ->
    Square = {'square', 2, rotate_array:make_test_square(4, 2)},
    Quad = #quad{ul=1, ur=2
                ,ll=3, lr=4
                },
    ?assertEqual(Quad, to_quad(Square)).

to_quad4_test() ->
    Square = {'square', 4, rotate_array:make_test_square(16, 4)},
    Quad = #quad{ul=#quad{ul=1, ur=2
                         ,ll=5, lr=6
                         }
                ,ur=#quad{ul=3, ur=4
                         ,ll=7, lr=8
                         }
                ,ll=#quad{ul=9, ur=10
                         ,ll=13, lr=14
                         }
                ,lr=#quad{ul=11, ur=12
                         ,ll=15, lr=16
                         }
                },
    ?assertEqual(Quad, to_quad(Square)).

to_square2_test() ->
    Square = {'square', 2, rotate_array:make_test_square(4, 2)},
    Quad = #quad{ul=1, ur=2
                ,ll=3, lr=4
                },
    ?assertEqual(Square, to_square(Quad)).

to_square4_test() ->
    Square = {'square', 4, rotate_array:make_test_square(16, 4)},
    Quad = #quad{ul=#quad{ul=1, ur=2
                         ,ll=5, lr=6
                         }
                ,ur=#quad{ul=3, ur=4
                         ,ll=7, lr=8
                         }
                ,ll=#quad{ul=9, ur=10
                         ,ll=13, lr=14
                         }
                ,lr=#quad{ul=11, ur=12
                         ,ll=15, lr=16
                         }
                },
    ?assertEqual(Square, to_square(Quad)).

proper_test_() ->
    {'timeout'
    ,10000
    ,[?_assertEqual([], proper:module(?MODULE, [{'to_file', 'user'}
                                               ,{'numtests', 5000}
                                               ]
                                     ))
     ]
    }.

prop_roundtrip() ->
    ?FORALL(Quad, quad(), Quad =:= to_quad(to_square(Quad))).

-endif.
