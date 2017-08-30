%% https://www.hackerrank.com/challenges/array-pairs/problem
%% Consider an array of N integers, A=[A1,A2,...An]. Find and print the
%% total number of (i,j) pairs such that Ai * Aj <= max(Ai, Ai+1...Aj)
%% where i < j.

%% Input Format

%% The first line contains an integer, N, denoting the number of
%% elements in the array.  The second line consists of space-separated
%% integers describing the respective values of A1, A2...An.

%% Constraints 1 <= N <= 5x10^5 1 <= Ai <= 10^9

%% Output Format

%% Print a long integer denoting the total number (i,j) pairs
%% satisfying Ai * Aj <= max(Ai, Ai+1...Aj) where i < j.

%% Sample Input

%% 5 1 1 2 4 2

%% Sample Output

%% 8

%% Explanation

%% There are eight pairs satisfying the given criteria: (1,2), (1,3),
%% (1,4), (1,5), (2,3), (2,4), (2,5), (3,5). Thus, we print 8 as our
%% answer.

-module(array_pairs).

-export([answer/0, answer/2
        ,main/0
        ]).

main() ->
    StrN = io:get_line(""),
    StrVs = io:get_line(""),

    N = to_n(StrN),
    Vs = to_vs(StrVs),
    Pairs = answer(N, Vs),
    io:format("~p~n", [Pairs]).

to_n(StrN) when is_list(StrN) ->
    list_to_integer(StrN -- [$\n]);
to_n(BinN) when is_binary(BinN) ->
    binary_to_integer(BinN).

to_vs(StrVs) when is_list(StrVs) ->
    [to_n(V) || V <- string:tokens(StrVs -- [$\n], " ")];
to_vs(BinVs) when is_binary(BinVs) ->
    [to_n(V) || V <- binary:split(BinVs, <<" ">>, ['global'])].

answer() ->
    N = 5,
    Vs = [1, 1, 2, 4, 2],
    answer(N, Vs).

answer(N, Vs) ->
    pairs(lists:zip(lists:seq(1, N), Vs)).

pairs([]) -> 0;
pairs([Ai | Js]) ->
    pairs(Ai, lists:reverse(Js), 0) + pairs(Js).

pairs(_, [], Pairs) -> Pairs;
pairs({I, Ai}, [{J, Aj} | Js], Pairs) ->
    {K, Ak} = kmax({I, Ai}, {J, Aj}, Js),
    case {I, K, J} of
        {I, I, J} when Aj =:= 1 ->
            pairs({I, Ai}, Js, Pairs+1);
        {I, I, J} ->
            pairs({I, Ai}, Js, Pairs);
        {I, J, J} when Ai =:= 1 ->
            pairs({I, Ai}, Js, Pairs+1);
        {I, J, J} ->
            pairs({I, Ai}, Js, Pairs);
        {I, K, J} when Ak >=(Ai*Aj) ->
            pairs({I, Ai}, Js, Pairs+1);
        _ ->
            pairs({I, Ai}, Js, Pairs)
    end.

kmax({I, Ai}, {_J, Aj}, Ks) when Ai > Aj ->
    kmax({I, Ai}, Ks);
kmax(_, J, Ks) ->
    kmax(J, Ks).

kmax({K, Ak}, []) -> {K, Ak};
kmax({_K, Ak}, [{N, An} | Ns]) when An > Ak ->
    kmax({N, An}, Ns);
kmax(K, [_|Ns]) ->
    kmax(K, Ns).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tc_test_() ->
    {'timeout', 240
    ,{'inparallel', filelib:fold_files(code:lib_dir('hackerrank', 'src'), "\\.txt", 'false', fun add_test/2, [])}
    }.

add_test(File, Tests) ->
    {'ok', Data} = file:read_file(File),
    [N, Vs, Answer|_] = binary:split(Data, <<"\n">>, ['global']),
    [{'timeout', 60, [{filename:basename(File), ?_assertEqual(to_n(Answer), answer(to_n(N), to_vs(Vs)))}]}
     | Tests
    ].
-endif.
