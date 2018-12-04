#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

%% Run as ./p3.escript

-export([main/1]).

main(_) ->
    Claims = claims(),
    P3_1 = p3_1(Claims),
    P3_2 = p3_2(Claims),
    io:format("p3_1: ~p~np3_2: ~s~n", [P3_1, P3_2]).

claims() ->
    {'ok', Bin} = file:read_file("p3.txt"),
    %% Bin = <<"#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n#4 @ 0,6: 2x2\n">>,
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
    {BaseClaim, calculate_overlap(Claim, BaseClaim) ++ Overlaps}.

calculate_overlap(#{'id' := _ID1, 'top' := T1, 'left' := L1, 'bottom' := B1, 'right' := R1}
                 ,#{'id' := _ID2, 'top' := T2, 'left' := L2, 'bottom' := B2, 'right' := R2}
                 ) ->
    %% In the problem's data:
    %% Claim #: {T, L} {B, R}
    %% Claim 1: {4, 2} {7, 5}
    %% Claim 2: {2, 4} {5, 7}

    Overlap = {{min(R1, R2), max(L1, L2)}
              ,{min(B1, B2), max(T1, T2)}
              },
    %% overlap: {{5,4},{5,4}}

    %% {Top, Left} is closest to 0,0
    %% {Bottom, Right} should be further away
    case Overlap of
        %% No overlaps, R is before L, T is below B
        {{R, L}, {T, B}} when R < L; T < B -> [];
        {{R, L}, {T, B}} ->
            %% We compute the intersection rectangle:
            ITop = max(T1, T2),
            ILeft = max(L1, L2),
            IBot = ITop + (T - B),
            IRight = ILeft + (R-L),

            %% intersection rect: {ITop, ILeft} {IBot, IRight}
            %% intersection rect: {4, 4} {5, 5}

            %% We add the intersection points to the list of overlaps
            [{X, Y} || X <- lists:seq(ILeft, IRight),
                      Y <- lists:seq(ITop, IBot)
            ]
    end.

%% We track the set of all Claim IDs and the set of all Claim IDs that have at least one overlapping claim
p3_2(Claims) ->
    find_unique_claim(Claims, sets:new(), sets:new()).

find_unique_claim([], AllIDs, OverlapIDs) ->
    %% Unique = {AllIds} - {OverlapIds}
    [Id] = sets:to_list(sets:subtract(AllIDs, OverlapIDs)),
    Id;
find_unique_claim([#{'id' := Id}=Claim | Claims], AllIDs, OverlapIDs) ->
    case find_overlaps(Claim, Claims) of
        %% If only the base claim is returned, this Id had no overlaps with the rest of the claims
        %% (may still have been overlapped by earlier claims
        [Id] -> find_unique_claim(Claims, sets:add_element(Id, AllIDs), OverlapIDs);

        %% If the Claim has overlaps, add all those IDs to the Overlap set
        Overlaps -> find_unique_claim(Claims, sets:add_element(Id, AllIDs), lists:foldl(fun sets:add_element/2, OverlapIDs, Overlaps))
    end.

find_overlaps(#{'id' := BaseId}=BaseClaim, Claims) ->
    %% foreach claim
    %%   if BaseClaim and Claim have an overlap, add Claim to the list
    lists:foldl(fun(#{'id' := ID}=Claim, Os) ->
                        case has_overlap(BaseClaim, Claim) of
                            'true' -> [ID | Os];
                            'false' -> Os
                        end
                end
                %% init the accumulator so BaseId is included if overlaps are found
               ,[BaseId]
               ,Claims
               ).

%% Two claims overlap if the their overlap is a non-empty list of coordinates
has_overlap(Claim1, Claim2) ->
    [] =/= calculate_overlap(Claim1, Claim2).
