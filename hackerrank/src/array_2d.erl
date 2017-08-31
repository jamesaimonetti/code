%% https://www.hackerrank.com/challenges/2d-array
%% Given a 6x6 2D Array, A:

%% 1 1 1 0 0 0
%% 0 1 0 0 0 0
%% 1 1 1 0 0 0
%% 0 0 0 0 0 0
%% 0 0 0 0 0 0
%% 0 0 0 0 0 0

%% We define an hourglass in A to be a subset of values with indices
%% falling in this pattern in A's graphical representation:

%% a b c
%%   d
%% e f g

%% There are 16 hourglasses in A, and an hourglass sum is the sum of an hourglass' values.

%% Task
%% Calculate the hourglass sum for every hourglass in A, then print the maximum hourglass sum.

%% Note: If you have already solved the Java domain's Java 2D Array challenge, you may wish to skip this challenge.

%% Input Format

%% There are 6 lines of input, where each line contains 6
%% space-separated integers describing 2D Array A; every value in A
%% will be in the inclusive range of -9 to 9.

%% Constraints

%% -9 <= A[i][j] <= 9
%% 0 <= i,j <= 5

%% Output Format

%% Print the largest (maximum) hourglass sum found in A.

%% Sample Input

%% 1 1 1 0 0 0
%% 0 1 0 0 0 0
%% 1 1 1 0 0 0
%% 0 0 2 4 4 0
%% 0 0 0 2 0 0
%% 0 0 1 2 4 0

%% Sample Output

%% 19

%% Explanation

%% A contains the following hourglasses:

%% 1 1 1   1 1 0   1 0 0   0 0 0
%%   1       0       0       0
%% 1 1 1   1 1 0   1 0 0   0 0 0

%% 0 1 0   1 0 0   0 0 0   0 0 0
%%   1       1       0       0
%% 0 0 2   0 2 4   2 4 4   4 4 0

%% 1 1 1   1 1 0   1 0 0   0 0 0
%%   0       2       4       4
%% 0 0 0   0 0 2   0 2 0   2 0 0

%% 0 0 2   0 2 4   2 4 4   4 4 0
%%   0       0       2       0
%% 0 0 1   0 1 2   1 2 4   2 4 0

%% The hourglass with the maximum sum (19) is:

%% 2 4 4
%%   2
%% 1 2 4

-module(array_2d).

-export([main/0]).

main() ->
    Array = read(6),
    MaxSum = max_sum(Array),
    io:format("~p~n", [MaxSum]).

max_sum(Array) ->
    max_sum(Array, 0).

max_sum([_], Max) -> Max;
max_sum([_, _], Max) -> Max;
max_sum([Row1, Row2, Row3 | Rest], Max) ->
    max_sum([Row2, Row3 | Rest], max_hourglass([Row1, Row2, Row3], Max)).

max_hourglass([[_,_]
              ,[_,_]
              ,[_,_]
              ]
             ,Max
             ) -> Max;
max_hourglass([[Row1A, Row1B, Row1C | Row1]
              ,[_Row2A, Row2B, Row2C | Row2]
              ,[Row3A, Row3B, Row3C | Row3]
              ]
             , Max) ->
    HourglassSum = Row1A + Row1B + Row1C
        + Row2B
        + Row3A + Row3B + Row3C,
    max_hourglass([[Row1B, Row1C | Row1]
                  ,[Row2B, Row2C | Row2]
                  ,[Row3B, Row3C | Row3]
                  ]
                 ,max(HourglassSum, Max)
                 ).

read(N) ->
    read(N, []).
read(0, Array) -> lists:reverse(Array);
read(N, Array) -> read(N-1, [to_vs(io:get_line("")) | Array]).

to_n(StrN) when is_list(StrN) ->
    list_to_integer(StrN -- [$\n]);
to_n(BinN) when is_binary(BinN) ->
    binary_to_integer(BinN).

to_vs(StrVs) when is_list(StrVs) ->
    [to_n(V) || V <- string:tokens(StrVs -- [$\n], " ")];
to_vs(BinVs) when is_binary(BinVs) ->
    [to_n(V) || V <- binary:split(BinVs, <<" ">>, ['global'])].
