#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

main(_) ->
    Ns = ns(),
    P1_1 = p1_1(Ns),
    P1_2 = p1_2(Ns),
    io:format("p1_1: ~p~np2_2: ~p~n", [P1_1, P1_2]).

-spec p1_1([integer()]) -> integer().
p1_1(Ns) ->
    lists:foldl(fun erlang:'+'/2, 0, Ns).

-spec p1_2([integer()]) -> integer().
p1_2(Ns) ->
    detect_repeat(Ns, Ns, 0, #{}).

detect_repeat(Ns, [], Freq, FreqMap) ->
    detect_repeat(Ns, Ns, Freq, FreqMap);
detect_repeat(AllNs, [N|Ns], Freq, FreqMap) ->
    NewFreq = Freq + N,
    case FreqMap of
        #{NewFreq := _} -> NewFreq;
        _Map -> detect_repeat(AllNs, Ns, NewFreq, FreqMap#{Freq => 'true'})
    end.

-spec ns() -> [integer()].
ns() ->
    {'ok', Bin} = file:read_file("p1.txt"),
    Inputs = binary:split(Bin, <<"\n">>, ['global']),
    [binary_to_integer(Input) || Input <- Inputs, byte_size(Input) > 0].
