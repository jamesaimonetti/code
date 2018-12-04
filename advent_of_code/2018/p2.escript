#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

%% Run as ./p2.escript

-export([main/1]).

main(_) ->
    IDs = ids(),
    P2_1 = p2_1(IDs),
    io:format("p2_1: ~p~n", [P2_1]).

ids() ->
    {'ok', Bin} = file:read_file("p2.txt"),
    binary:split(Bin, <<"\n">>, ['global']).

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
