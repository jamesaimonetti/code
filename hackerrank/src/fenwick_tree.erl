-module(fenwick_tree).

%% AKA Binary Indexed Tree
-export([bindex/1, bindex/2
        ,sum/2
        ,product/2
        ,add/3
        ]).

-define(BINDEX(Len, List), {'bindex', Len, List}).
-define(BINDEX_LEN(Len), {'bindex', Len, _List}).
-define(BINDEX_LIST(List), {'bindex', _Len, List}).

bindex(N) when is_integer(N), N > 0 ->
    bindex(N, lists:duplicate(N, 0));
bindex(List) when is_list(List) ->
    bindex(length(List), List).
bindex(N, List) ->
    Bindex = lists:zip(lists:seq(1, N), List),
    bindex_init(?BINDEX(N, Bindex), 1).

bindex_init(?BINDEX(Len, Bindex), Len) ->
    ?BINDEX(Len, lists:keysort(1, Bindex));
bindex_init(Bindex, Index) ->
    bindex_init(Bindex, Index, add_last_set_bit(Index)).

bindex_init(?BINDEX(Len, Bindex), Index, Index2) when Index2 < Len ->
    {Index, Value} = lists:keyfind(Index, 1, Bindex),
    {'value', {Index2, Value2}, Bindex1} = lists:keytake(Index2, 1, Bindex),
    bindex_init(?BINDEX(Len, [{Index2, Value2 + Value} | Bindex1]), Index+1);
bindex_init(Bindex, Index, _Index2) ->
    bindex_init(Bindex, Index+1).

sum(?BINDEX_LIST(Bindex), Nth) ->
    sum(Bindex, Nth, 0).

sum(_Bindex, 0, Sum) -> Sum;
sum(Bindex, Nth, Sum) ->
    {Nth, H} = lists:keyfind(Nth, 1, Bindex),
    sum(Bindex, remove_last_set_bit(Nth), Sum+H).

product(?BINDEX_LIST(Bindex), Index) ->
    product(Bindex, Index, 1).

product(_Bindex, 0, Product) -> Product;
product(Bindex, Index, Product) ->
    {Index, H} = lists:keyfind(Index, 1, Bindex),
    product(Bindex, remove_last_set_bit(Index), Product*H).

add(?BINDEX(Len, Bindex), Index, Val) when Index < Len ->
    {'value', {Index, V}, Bindex1} = lists:keytake(Index, 1, Bindex),
    add(?BINDEX(Len, [{Index, V + Val} | Bindex1]), add_last_set_bit(Index), Val);
add(?BINDEX(Len, Bindex), _Index, _Val) -> ?BINDEX(Len, lists:keysort(1, Bindex)).

remove_last_set_bit(N) ->
    N - (N band (-N)).

add_last_set_bit(N) ->
    N + (N band (-N)).
