#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

main(_) ->
    %% Read in the numbers from the file
    Ns = ns(),

    %% Calculate the answers
    P1_1 = p1_1(Ns),
    P1_2 = p1_2(Ns),

    %% Report the answers
    io:format("p1_1: ~p~np2_2: ~p~n", [P1_1, P1_2]).

-spec p1_1([integer()]) -> integer().
p1_1(Ns) ->
    %% Fold over the list of integers, using the addition function to sum them
    %% Would probably use lists:sum/1 in a production environment but I like the fold
    %% Shows how to use a named function when the list element and accumulater line up nicely in the function args
    lists:foldl(fun erlang:'+'/2, 0, Ns).

-spec p1_2([integer()]) -> integer().
p1_2(Ns) ->
    %% We need to keep the original list of numbers around for when we exhaust the list during a pass
    %% We start at 0 for the Frequency
    %% We keep a map of "seen" numbers - rudimentary memoization
    detect_repeat(Ns, Ns, 0, #{}).

%% We've reached the end of the list of numbers, loop the original list back in
detect_repeat(Ns, [], Freq, FreqMap) ->
    detect_repeat(Ns, Ns, Freq, FreqMap);

detect_repeat(AllNs, [N|Ns], Freq, FreqMap) ->
    NewFreq = Freq + N,

    %% Check the FreqMap for NewFreq
    case FreqMap of
        %% If we've seen it, we're done
        #{NewFreq := _} -> NewFreq;
        %% Otherwise add the new frequency to the list and loop
        _Map -> detect_repeat(AllNs, Ns, NewFreq, FreqMap#{Freq => 'true'})
    end.

-spec ns() -> [integer()].
ns() ->
    {'ok', Bin} = file:read_file("p1.txt"), % Bin = <<"+14\n-15\n...\n">>
    Inputs = binary:split(Bin, <<"\n">>, ['global']), % Inputs = [<<"+14">>, <<"-15">>,...<<>>]

    %% We take advantage of the fact that binary_to_integer/1 deals with +/- signs appropriately
    %% as well as strips the empty binary off the end
    %% returns [15, -14,...]
    [binary_to_integer(Input) || Input <- Inputs, byte_size(Input) > 0].
