%% https://www.hackerrank.com/challenges/arrays-ds
%% An array is a type of data structure that stores elements of the
%% same type in a contiguous block of memory. In an array, A, of size
%% N, each memory location has some unique index, i (where 0 <= i < N),
%% that can be referenced as A[i] (you may also see it written as Ai).

%% Given an array, A, of N integers, print each element in reverse
%% order as a single line of space-separated integers.

%% Note: If you've already solved our C++ domain's Arrays Introduction
%% challenge, you may want to skip this.

%% Input Format

%% The first line contains an integer, N (the number of integers in A).
%% The second line contains N space-separated integers describing A.

%% Constraints
%% 1 <= N <= 10^3
%% 1 <= Ai <= 10^4

%% Output Format

%% Print all N integers in A in reverse order as a single line of
%% space-separated integers.

%% Sample Input
%% 4
%% 1 4 3 2

%% Sample Output
%% 2 3 4 1
-module(arrays_ds).

-export([main/0]).

main() ->
    _StrN = io:get_line(""),
    StrVs = io:get_line(""),

    [H|Vs] = lists:reverse(to_vs(StrVs)),

    io:format("~w", [H]),
    [io:format(" ~w", [V]) || V <- Vs],
    io:format("~n", []).

to_n(StrN) when is_list(StrN) ->
    list_to_integer(StrN -- [$\n]);
to_n(BinN) when is_binary(BinN) ->
    binary_to_integer(BinN).

to_vs(StrVs) when is_list(StrVs) ->
    [to_n(V) || V <- string:tokens(StrVs -- [$\n], " ")];
to_vs(BinVs) when is_binary(BinVs) ->
    [to_n(V) || V <- binary:split(BinVs, <<" ">>, ['global'])].
