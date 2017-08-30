-module(p43).

-export([answer/0]).

%% The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each
%% of the digits 0 to 9 in some order, but it also has a rather interesting sub-string
%% divisibility property.
%% Let d_(1) be the 1^(st) digit, d_(2) be the 2^(nd) digit, and so on. In this way,
%% we note the following:
%%    * d_(2)d_(3)d_(4)=406 is divisible by 2
%%    * d_(3)d_(4)d_(5)=063 is divisible by 3
%%    * d_(4)d_(5)d_(6)=635 is divisible by 5
%%    * d_(5)d_(6)d_(7)=357 is divisible by 7
%%    * d_(6)d_(7)d_(8)=572 is divisible by 11
%%    * d_(7)d_(8)d_(9)=728 is divisible by 13
%%    * d_(8)d_(9)d_(10)=289 is divisible by 17
%% Find the sum of all 0 to 9 pandigital numbers with this property.

answer() ->
    Ps = primes:queue(17),
    Pans = lists:filter(fun(P) -> is_valid_pan(tl(P), Ps) end, variations(9)),
    lists:sum([ list_to_integer(P) || P <- Pans]).

is_valid_pan(_, []) -> true;
is_valid_pan([D1, D2, D3 | Ds], [P | Ps]) ->
    case list_to_integer([D1, D2, D3]) rem P =:= 0 of
        true -> is_valid_pan([D2, D3 | Ds], Ps);
        false -> false
    end.

% all pandigital variations
variations(N) ->
    lists:map(fun(L) -> lists:map(fun(Ns) -> Ns + $0 end, L) end, my_math:perms(lists:seq(N,0,-1))).
    
