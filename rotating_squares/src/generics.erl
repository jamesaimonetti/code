-module(generics).

-export([multirec/5
        ,pmultirec/5
        ]).

multirec(Indivisable, Value, Divide, Combine, Data) ->
    case Indivisable(Data) of
        'true' -> Value(Data);
        'false' ->
            Parts = Divide(Data),
            MappedParts = lists:map(fun(Part) -> multirec(Indivisable, Value, Divide, Combine, Part) end, Parts),
            Combine(MappedParts)
    end.


pmultirec(Indivisable, Value, Divide, Combine, Data) ->
    case Indivisable(Data) of
        'true' -> Value(Data);
        'false' ->
            Parts = Divide(Data),
            MappedParts = pmap(fun(Part) -> pmultirec(Indivisable, Value, Divide, Combine, Part) end, Parts),
            Combine(MappedParts)
    end.

pmap(Fun, List) ->
    {PidRefs, _} = lists:foldl(fun(Item, Acc) -> pmap_fold(Item, Acc, Fun) end
                              ,{[], 0}
                              ,List
                              ),
    pmap_wait(PidRefs).

pmap_fold(Item, {Acc, N}, Fun) ->
    Self = self(),
    PidRef = spawn_monitor(fun() -> Self ! {N+1, Fun(Item)} end),
    {[{PidRef, N+1} | Acc], N+1}.

pmap_wait(PidRefs) ->
    pmap_wait(PidRefs, []).

pmap_wait([], Results) ->
    [Result || {Result, _} <- lists:keysort(2, Results)];
pmap_wait([{{Pid, Ref}, N} | PidRefs], Results) ->
    receive
        {N, Result} ->
            erlang:demonitor(Ref, ['flush']),
            pmap_wait(PidRefs, [{Result, N} | Results]);
        {'DOWN', Ref, 'process', Pid, 'normal'} ->
            pmap_wait(PidRefs, Results);
        {'DOWN', Ref, 'process', Pid, Reason} ->
            throw({'error', Reason})
    end.
