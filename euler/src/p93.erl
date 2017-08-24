-module(p93).

-export([answer/0
        ,targets/1
        ]).

%% By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses, it is possible to form different positive integer targets.

%% For example,

%% 8 = (4 * (1 + 3)) / 2
%% 14 = 4 * (3 + 1 / 2)
%% 19 = 4 * (2 + 3) − 1
%% 36 = 3 * 4 * (2 + 1)

%% Note that concatenations of the digits, like 12 + 34, are not allowed.

%% Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.

%% Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd.

answer() ->
    Ns = lists:seq(1, 9),
    ABCDs = [{A, B, C, D}
             || A <- Ns,
               B <- Ns,
               C <- Ns,
               D <- Ns,
               A < B andalso B < C andalso C < D
            ],
    [{ABCD, targets(ABCD)} || ABCD <- ABCDs].

targets({A, B, C, D}=ABCD) ->
    Ops = [fun erlang:'+'/2
          ,fun erlang:'-'/2
          ,fun erlang:'*'/2
          ,fun erlang:'div'/2
          ],
    Operations = [{Op1, Op2, Op3}
                  || Op1 <- Ops,
                    Op2 <- Ops,
                    Op3 <- Ops
                 ],
    lists:keysort(1, [{Answer, to_string(ABCD, Op)}
                      || {Op1, Op2, Op3}=Op <- Operations,
                        Answer <- [Op3(Op2(Op1(A, B), C), D)],
                        Answer > 0
                     ]
                 ).

to_string({A, B, C, D}, {Op1, Op2, Op3}) ->
    lists:flatten([to_string(D), " ", to_string(Op3), " (", to_string(C), " ", to_string(Op2), " (", to_string(B), " ", to_string(Op1), " ", to_string(A), ") )"]).

to_string(N) when is_integer(N) -> integer_to_list(N);
to_string(fun erlang:'+'/2) -> "+";
to_string(fun erlang:'-'/2) -> "-";
to_string(fun erlang:'*'/2) -> "*";
to_string(fun erlang:'div'/2) -> "/".
