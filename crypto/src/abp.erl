-module(abp).

-export([solve/0
        ,solve/1
        ,fit/1

        ,abp/0, abp/1
        ]).

-define(INT128, 128).
-define(INT128_SIZE, 340282366920938463463374607431768211456).
-define(INT128_BYTES, 384).

-define(INT64, 64).
-define(INT64_SIZE, 18446744073709551616).
-define(INT64_BYTES, 192).

-define(INT32, 32).
-define(INT32_SIZE, 4294967296).
-define(INT32_BYTES, 96).

-define(BYTES, ?INT128_BYTES).
-define(SIZE, ?INT128).
-define(MAX_SIZE, ?INT128_SIZE).

-define(GENERATIONS, 1000).

-spec abp() ->
                 'undefined' |
                 {pos_integer(), pos_integer(), pos_integer()}.
-spec abp('undefined' | binary() | tuple()) ->
                 'undefined' |
                 {pos_integer(), pos_integer(), pos_integer()}.
abp() ->
    abp(get('fittest')).

abp('undefined') -> 'undefined';
abp(<<Apple:?INT128/integer, Banana:?INT128/integer, Pineapple:?INT128/integer>>) ->
    {Apple, Banana, Pineapple};
abp(<<Apple:?INT64/integer, Banana:?INT64/integer, Pineapple:?INT64/integer>>) ->
    {Apple, Banana, Pineapple};
abp(<<Apple:?INT32/integer, Banana:?INT32/integer, Pineapple:?INT32/integer>>) ->
    {Apple, Banana, Pineapple};
abp({'give_up', _Runs, _Fitness, ABP}) ->
    abp(ABP);
abp({'found', _Generation, ABP}) ->
    abp(ABP).

-spec solve() -> 'ok'.
-spec solve(integer()) -> 'ok'.
solve() ->
    rand:seed(exs1024),
    solve(?GENERATIONS).
solve(Max) ->
    case get('fittest') of
        <<_:?SIZE/integer, _:?SIZE/integer, _:?SIZE/integer>>=Fittest ->
            solve(1, Max, [Fittest | gene_pool()]);
        _ ->
            solve(1, Max, gene_pool())
    end.

-define(POPULATION, 40).
gene_pool() ->
    [gene() || _ <- lists:seq(1, ?POPULATION)].

gene() ->
    <<(rand:uniform(?MAX_SIZE)):?SIZE/integer
      ,(rand:uniform(?MAX_SIZE)):?SIZE/integer
      ,(rand:uniform(?MAX_SIZE)):?SIZE/integer
    >>.

-define(GOAL, 4.0).

solve(Generation, Max, GenePool) ->
    case sort_pool(GenePool) of
        [] -> solve(Generation+1, Max, mutate(GenePool));
        [{_Distance, Fitness, Fittest}] ->
            solve(Generation, Max, Fitness, Fittest, hd(GenePool));
        [{_Distance, Fitness, Fittest}
        ,{_, _, NextBestest}
         | _Rest
        ] ->
            solve(Generation, Max, Fitness, Fittest, NextBestest)
    end.

solve(Max, Max, Fitness, Fittest, _) ->
    {'give_up', Max, Fitness, Fittest};
solve(Generation, _Max, Fitness, Fittest, _NextBestest)
  when Fitness == ?GOAL ->
    {'found', Generation, Fittest};
solve(Generation, Max, _Fitness, Fittest, NextBestest) ->
    _ = put('fittest', Fittest),
    NewPool = [Fittest | crossover(Fittest, NextBestest)],
    solve(Generation+1, Max, mutate_pool(NewPool)).

sort_pool(GenePool) ->
    Calcd = [C || {_, Fitness, _}=C <-
                      plists:map(fun(Gene) ->
                                         Fitness = fit(Gene),
                                         {distance(Fitness), Fitness, Gene}
                                 end
                                ,GenePool
                                ,?POPULATION div 10
                                ),
                  Fitness =/= -1
            ],
    lists:keysort(1, Calcd).

crossover(G1, G2) ->
    {_, _, Acc} = lists:foldl(fun crossover_fold/2, {G1, G2, []}, lists:seq(1, ?POPULATION div 2)),
    Acc.

crossover_fold(_, {G1, G2, Pool}) ->
    {G1Prime, G2Prime} = procreate(G1, G2),
    {G1, G2, [G1Prime, G2Prime | Pool]}.

procreate(G1, G2) ->
    Point = rand:uniform(?BYTES-3),
    Point2 = rand:uniform(?BYTES - Point - 1),
    RestLen = ?BYTES - Point - Point2,

    <<G1Part1:Point/bitstring, G1Part2:Point2/bitstring, G1Part3:RestLen/bitstring>> = G1,
    <<G2Part1:Point/bitstring, G2Part2:Point2/bitstring, G2Part3:RestLen/bitstring>> = G2,

    {<<G1Part1:Point/bitstring, G2Part2:Point2/bitstring, G1Part3:RestLen/bitstring>>
    ,<<G2Part1:Point/bitstring, G1Part2:Point2/bitstring, G2Part3:RestLen/bitstring>>
    }.

mutate_pool(Pool) ->
    [mutate_gene(Gene) || Gene <- Pool].

mutate_gene(Gene) ->
    << <<(mutate(X)):?SIZE/integer>> || <<X:?SIZE/integer>> <= Gene >>.

mutate(X) ->
    case rand:uniform() < 0.1 of
        'true' -> up_or_down(X);
        'false' -> X
    end.

up_or_down(X) ->
    up_or_down(X, rand:uniform(2), rand:uniform(?MAX_SIZE)).
up_or_down(X, 1, Factor) -> (X-Factor) div ?MAX_SIZE;
up_or_down(X, 2, Factor) -> (X+Factor) div ?MAX_SIZE.

distance(Fitness) ->
    abs(?GOAL - Fitness).

-spec fit({pos_integer(), pos_integer(), pos_integer()}) -> float().
fit(<<Apple:?SIZE/integer, Banana:?SIZE/integer, Pineapple:?SIZE/integer>>) ->
    fit(Apple, Banana, Pineapple).

fit(0, 0, _) -> -1;
fit(_, 0, 0) -> -1;
fit(0, _, 0) -> -1;
fit(Apple, Banana, Pineapple) ->
    (Apple / (Banana + Pineapple))
        + (Banana / (Apple + Pineapple))
        + (Pineapple / (Apple + Banana)).
