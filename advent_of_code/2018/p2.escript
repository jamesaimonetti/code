#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

%% Run as ./p2.escript

-export([main/1]).

main(_) ->
    IDs = ids(),
    P2_1 = p2_1(IDs),
    P2_2 = p2_2(IDs),
    io:format("p2_1: ~p~np2_2: ~s~n", [P2_1, P2_2]).

ids() ->
    {'ok', Bin} = file:read_file("p2.txt"),
    %% Just read in IDs, one per line
    [ID || ID <- binary:split(Bin, <<"\n">>, ['global']), byte_size(ID) > 0].

p2_1(IDs) ->
    {Twos, Threes} = lists:foldl(fun count_repetitions/2, {0, 0}, IDs),
    Twos * Threes.

count_repetitions(ID, {_Twos, _Threes}=Acc) ->
    %% We can sort the ID from "abcabc" to "aabbcc"
    Chars = lists:sort(binary_to_list(ID)),
    %% This sorted list makes counting repetitions easier
    Counts = count_chars(Chars),

    %% We only need to know if we have one or more characters repeated 2 or 3 times
    %% usort will take [1,2,4,3,2] and make [1,2,3,4]
    update_counts(lists:usort(Counts), Acc).

%%% since we know 2 and 3 appear at most once, increment their counters if encountered
update_counts([], Acc) -> Acc;
update_counts([2|Counts], {Twos, Threes}) -> update_counts(Counts, {Twos+1, Threes});
update_counts([3|Counts], {Twos, Threes}) -> update_counts(Counts, {Twos, Threes+1});
update_counts([_Count|Counts], Acc) -> update_counts(Counts, Acc).

count_chars([]) -> [];
count_chars([C|Chars]) ->
    %% we seed the accumulator with the first char, count of 1
    {LastChar, LastCount, Counts} = lists:foldl(fun count_char/2, {C, 1, []}, Chars),

    %% we need to add the last char's count to the list
    %% lists:unzip will take [{Char, Count}] and split it into [Char], [Count] lists
    {_Chars, CharCounts} = lists:unzip([{LastChar, LastCount} | Counts]),

    %% just return the unordered counts
    CharCounts.

%% If the same character, increment its count
count_char(Char, {Char, Count, Counts}) ->
    {Char, Count+1, Counts};
%% if a new character, add the previous character's count and start the new counter
count_char(NewChar, {OldChar, Count, Counts}) ->
    {NewChar, 1, [{OldChar, Count}|Counts]}.

p2_2(IDs) ->
    %% since we know the ID we're searching for differs by a single character
    %% we know that the sums of the IDs will not differ by more than 26 ($z-$a)
    %% this will shrink the number of comparisons we need to perform
    Summed = lists:keysort(1, [{lists:sum(binary_to_list(S)), S} || S <- IDs]),
    find_neighbors(Summed).

find_neighbors([ID1 | IDs]) ->
    %% take the head and search it against the rest of the list
    case find_neighbors(ID1, IDs) of
        'false' -> find_neighbors(IDs);
        Common -> Common
    end.

find_neighbors(_ID, []) -> 'false';
%% if the difference between the two Sums is less than 26, compare the IDs
find_neighbors({Sum1, ID1}, [{Sum2, ID2} | IDs]) when Sum2 - Sum1 < 26 ->
    case one_char_off(ID1, ID2) of
        'false' -> find_neighbors({Sum1, ID1}, IDs);
        Common -> Common
    end;
%% if the difference is greater than or equal to 26, skip the rest of the sums, no matches will be found
find_neighbors(_ID1, _IDs) -> 'false'.

one_char_off(ID1, ID2) ->
    Size = byte_size(ID1),

    %% Conveniently we can get the longest prefix and suffix shared by the IDs
    Suffix = binary:longest_common_suffix([ID1, ID2]),
    Prefix = binary:longest_common_prefix([ID1, ID2]),

    case Size - (Prefix + Suffix) of
        1 -> common_chars(Prefix, Suffix, ID1); % found ID pair
        _ -> 'false'
    end.

common_chars(PrefixCount, SuffixCount, ID) ->
    %% Using the prefix and suffix lengths, remove the unshared character and return the common characters
    <<Prefix:PrefixCount/binary, _, Suffix:SuffixCount/binary>> = ID,
    <<Prefix/binary, Suffix/binary>>.
