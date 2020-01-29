-module(cp_util).

-export([hex_to_base64/1
        ,fixed_xor/2
        ,from_hex/1, to_hex/1
        ]).

hex_to_base64(HexBin) ->
    Str = from_hex(HexBin, []),
    base64:encode(Str).

fixed_xor(Buffer1, Buffer2) ->
    Str1 = from_hex(Buffer1, []),
    Str2 = from_hex(Buffer2, []),
    to_hex(crypto:exor(Str1, Str2)).

from_hex(Buffer) ->
    from_hex(Buffer, []).

from_hex(<<>>, Acc) -> lists:reverse(Acc);
from_hex(<<Div, Rem, T/binary>>, Acc) ->
    Lo = hex_char_to_binary(Rem),
    Hi = hex_char_to_binary(Div),

    Sum = (Hi * 16) + Lo,

    from_hex(T, [Sum|Acc]).

-spec hex_char_to_binary(pos_integer()) -> pos_integer().
hex_char_to_binary(B) when B < 58 ->
    to_lower_char(B) - $0;
hex_char_to_binary(B) ->
    to_lower_char(B) - ($a - 10).

to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts Latin capital letters to lowercase, skipping 16#D7 (extended ASCII 215) "multiplication sign: x"
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
to_lower_char(C) -> C.

to_hex(Bin) ->
    << <<(to_hex_char(B div 16)), (to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

to_hex_char(N) when N < 10 -> $0 + N;
to_hex_char(N) when N < 16 -> $a - 10 + N.
