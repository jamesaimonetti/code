-module(s1_tests).

-include_lib("eunit/include/eunit.hrl").

%% https://cryptopals.com/sets/1/challenges/1
%% Convert hex to base64

%% The string:

%% 49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d

%% Should produce:

%% SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t

%% So go ahead and make that happen. You'll need to use this code for
%% the rest of the exercises.

%% Cryptopals Rule
%% Always operate on raw bytes, never on encoded strings. Only use hex
%% and base64 for pretty-printing.
ch1_test() ->
    Hex = <<"49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d">>,
    Base64 = <<"SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t">>,
    {"s1=>c1 hex_to_base64", ?assertEqual(Base64, cp_util:hex_to_base64(Hex))}.

%% https://cryptopals.com/sets/1/challenges/2
%%
%% Fixed XOR

%% Write a function that takes two equal-length buffers and produces
%% their XOR combination.

%% If your function works properly, then when you feed it the string:

%% 1c0111001f010100061a024b53535009181c

%% ... after hex decoding, and when XOR'd against:

%% 686974207468652062756c6c277320657965

%% ... should produce:

%% 746865206b696420646f6e277420706c6179
ch2_test() ->
    Buffer1 = <<"1c0111001f010100061a024b53535009181c">>,
    Buffer2 = <<"686974207468652062756c6c277320657965">>,
    XORed = <<"746865206b696420646f6e277420706c6179">>,

    {"s1=>c1 fixed_xor", ?assertEqual(XORed, cp_util:fixed_xor(Buffer1, Buffer2))}.
