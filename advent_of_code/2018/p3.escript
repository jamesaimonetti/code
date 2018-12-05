#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

%% Run as ./p3.escript

-export([main/1]).

main(_) ->
    Claims = claims(),
    P3_1 = p3_1(Claims),
    io:format("p3_1: ~p~n", [P3_1]).

claims() ->
    {'ok', Bin} = file:read_file("p3.txt"),
    %% Bin = <<"#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 4,4: 3x3\n#4 @ 0,6: 2x2\n">>,
    [claim(Claim) || Claim <- binary:split(Bin, <<"\n">>, ['global']), byte_size(Claim) > 0].

claim(ClaimBin) ->
    claim(ClaimBin, re:run(ClaimBin, <<"^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$">>, [{'capture', 'all_but_first', 'binary'}])).

claim(_ClaimBin, {'match', [ClaimId, OffsetLeftEdge, OffsetTopEdge, WidthBin, HeightBin]}) ->
    Left = binary_to_integer(OffsetLeftEdge)+1,
    Top = binary_to_integer(OffsetTopEdge)+1,
    Width = binary_to_integer(WidthBin),
    Height = binary_to_integer(HeightBin),

    #{'id' => ClaimId
    ,'left' => Left
    ,'right' => Left + Width - 1
    ,'top' => Top
    ,'bottom' => Top + Height - 1
     %% ,'width' => Width
     %% ,'height' => Height
     %% ,'area' => Width * Height
    }.

p3_1(Claims) ->
    p3_1(Claims, []).

%% Overlaps will be the points on the grid that are overlapped
p3_1([], Overlaps) ->
    %% Since a more than 2 claims can overlap a given point, that point may appear multiple times
    %% lists:usort/1 will remove duplicates, then the length will tell us the total overlap
    length(lists:usort(Overlaps));
p3_1([Claim | Claims], Overlaps) ->
    %% Compare the head claim against all remaining claims
    {_Claim, Updated} = lists:foldl(fun add_overlap/2, {Claim, Overlaps}, Claims),

    %% Compare the rest of the claims against each other
    p3_1(Claims, Updated).

add_overlap(Claim, {BaseClaim, Overlaps}) ->
    {BaseClaim, calculate_overlap(Claim, BaseClaim, Overlaps)}.

calculate_overlap(#{'id' := _ID1, 'top' := T1, 'left' := L1, 'bottom' := B1, 'right' := R1}
                 ,#{'id' := _ID2, 'top' := T2, 'left' := L2, 'bottom' := B2, 'right' := R2}
                 ,Overlaps
                 ) ->
    %% In the problem's data:
    %% Claim #: {T, L} {B, R}
    %% Claim 1: {4, 2} {7, 5}
    %% Claim 2: {2, 4} {5, 7}

    Overlap = {{min(R1, R2), max(L1, L2)}
              ,{min(B1, B2), max(T1, T2)}
              },
    %% overlap: {{5,4},{5,4}}

    case Overlap of
        %% No overlaps
        {{R, L}, {T, B}} when R < L, T < B -> Overlaps;

        %% Overlap on a single point
        {{X, X}, {Y, Y}} -> [{X, Y} | Overlaps];

        %% No X overlap
        {{R, L}, _Y} when R < L -> Overlaps;

        %% No Y overlap
        {_X, {T, B}} when T < B -> Overlaps;

        {{R, L}, {T, B}} ->
            %% We compute the intersection rectangle:
            ITop = max(T1, T2),
            ILeft = max(L1, L2),
            IBot = ITop + (T - B),
            IRight = ILeft + (R-L),

            %% intersection rect: {ITop, ILeft} {IBot, IRight}
            %% intersection rect: {4, 4} {5, 5}

            %% We add the intersection points to the list of overlaps
            XYs = [{X, Y} || X <- lists:seq(ILeft, IRight),
                            Y <- lists:seq(ITop, IBot)
                  ],
            XYs ++ Overlaps
    end.
