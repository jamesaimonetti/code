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
    [ID || ID <- binary:split(Bin, <<"\n">>, ['global']), byte_size(ID) > 0].

p2_1(IDs) ->
    {Twos, Threes} = lists:foldl(fun count_repetitions/2, {0, 0}, IDs),
    Twos * Threes.

count_repetitions(ID, {_Twos, _Threes}=Acc) ->
    Chars = lists:sort(binary_to_list(ID)),
    Counts = count_chars(Chars),
    update_counts(lists:usort(Counts), Acc).

update_counts([], Acc) -> Acc;
update_counts([2|Counts], {Twos, Threes}) -> update_counts(Counts, {Twos+1, Threes});
update_counts([3|Counts], {Twos, Threes}) -> update_counts(Counts, {Twos, Threes+1});
update_counts([_Count|Counts], Acc) -> update_counts(Counts, Acc).

count_chars([]) -> [];
count_chars([C|Chars]) ->
    {LastChar, LastCount, Counts} = lists:foldl(fun count_char/2, {C, 1, []}, Chars),
    {_Chars, CharCounts} = lists:unzip([{LastChar, LastCount} | Counts]),
    CharCounts.

count_char(Char, {Char, Count, Counts}) ->
    {Char, Count+1, Counts};
count_char(NewChar, {OldChar, Count, Counts}) ->
    {NewChar, 1, [{OldChar, Count}|Counts]}.

p2_2(IDs) ->
    Summed = lists:keysort(1, [{lists:sum(binary_to_list(S)), S} || S <- IDs]),
    find_neighbors(Summed).

find_neighbors([ID1 | IDs]) ->
    case find_neighbors(ID1, IDs) of
        'false' -> find_neighbors(IDs);
        Common -> Common
    end.

find_neighbors(_ID, []) -> 'false';
find_neighbors({Sum1, ID1}, [{Sum2, ID2} | IDs]) when Sum2 - Sum1 < $z ->
    case one_char_off(ID1, ID2) of
        'false' -> find_neighbors({Sum1, ID1}, IDs);
        Common -> Common
    end.


one_char_off(ID1, ID2) ->
    Size = byte_size(ID1),

    Suffix = binary:longest_common_suffix([ID1, ID2]),
    Prefix = binary:longest_common_prefix([ID1, ID2]),

    case Size - (Prefix + Suffix) of
        1 -> common_chars(Prefix, Suffix, ID1);
        _ -> 'false'
    end.

common_chars(PrefixCount, SuffixCount, ID) ->
    <<Prefix:PrefixCount/binary, _, Suffix:SuffixCount/binary>> = ID,
    <<Prefix/binary, Suffix/binary>>.
