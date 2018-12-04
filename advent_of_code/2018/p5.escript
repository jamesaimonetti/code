#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

main(_) ->
    Polymer = polymer(),
    P5_1 = p5_1(Polymer),
    P5_2 = p5_2(Polymer),
    io:format("p5_1: ~p~np5_2: ~p~n", [P5_1, P5_2]).

p5_1(Polymer) ->
    Result = reaction(Polymer, [], 'false'),
    byte_size(Result).

%% no reactions occurred; we have our steady-state polymer
reaction(<<>>, Result, 'false') ->
    list_to_binary(lists:reverse(Result));
%% if we had one or more reactions, build the new polymer and take another reaction pass
reaction(<<>>, Result, 'true') ->
    reaction(list_to_binary(lists:reverse(Result)), [], 'false');

%% stupid endlines
reaction(<<"\n", Rest/binary>>, Result, Reacted) ->
    reaction(Rest, Result, Reacted);

%% we know that $a-$A = 32, $b-$B = 32, etc
%% thus reverse polarities are just adjacent chars differing by 32
reaction(<<Little, Big, Rest/binary>>, Result, _Reacted) when abs(Little - Big) =:= 32 ->
    reaction(Rest, Result, 'true');

%% collect the unreacted unit in result
reaction(<<Unit, Rest/binary>>, Result, Reacted) ->
    reaction(Rest, [Unit | Result], Reacted).

p5_2(Polymer) ->
    Units = lists:seq($a, $z),
    {_, Length} = lists:foldl(fun find_bad_unit/2, {Polymer, byte_size(Polymer)}, Units),
    Length.

find_bad_unit(Unit, {Polymer, SmallestLength}) ->
    Shortened = remove_unit(Unit, Polymer),
    Reacted = reaction(Shortened, [], 'false'),
    case byte_size(Reacted) of
        ReactedSize when ReactedSize < SmallestLength -> {Polymer, ReactedSize};
        _Size -> {Polymer, SmallestLength}
    end.

%% remove all instances of the unit and its opposite polarity
remove_unit(Unit, Polymer) ->
    << <<U>> || <<U>> <= Polymer, U =/= Unit, U =/= (Unit - 32) >>.


polymer() ->
    {'ok', Bin} = file:read_file("p5.txt"),
    %% Bin = <<"dabAcCaCBAcCcaDA">>,
    Bin.
