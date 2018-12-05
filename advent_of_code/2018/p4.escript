#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-
-mode('compile').

-export([main/1]).

main(_) ->
    GuardTimes = guard_times(),
    P4_1 = p4_1(GuardTimes),
    io:format("p4_1: ~p~n", [P4_1]).

p4_1(GuardTimes) ->
    SleepyGuard = find_sleepiest_guard(GuardTimes),
    SleepiestMinute = find_sleepiest_minute(GuardTimes, SleepyGuard),
    SleepyGuard * SleepiestMinute.

find_sleepiest_guard([{Guard, _Time, 'awake'} | GuardTimes]) ->
    find_sleepiest_guard(GuardTimes, {Guard, #{}}).

find_sleepiest_guard([], {_Guard, SleepCounters}) ->
    [{Guard, _Time} | _] = lists:reverse(lists:keysort(2, maps:to_list(SleepCounters))),
    Guard;
find_sleepiest_guard([{Guard, StartTime, 'asleep'}, {Guard, EndTime, 'awake'} | GuardTimes]
                    ,{Guard, SleepCounters}
                    ) ->
    Elapsed = elapsed_minutes(EndTime, StartTime),

    find_sleepiest_guard(GuardTimes, {Guard, maps:update_with(Guard, fun(V) -> V+Elapsed end, Elapsed, SleepCounters)});
find_sleepiest_guard([{NewGuard, _StartTime, 'awake'} | GuardTimes]
                    ,{_OldGuard, SleepCounters}
                    ) ->
    find_sleepiest_guard(GuardTimes, {NewGuard, SleepCounters}).

find_sleepiest_minute(GuardTimes, SleepyGuard) ->
    GuardSchedule = lists:keysort(1, [{Time, State} || {G, Time, State} <- GuardTimes, G =:= SleepyGuard]),

    Minutes = guard_sleep_schedule(GuardSchedule, #{}),
    [{Minute, _Slept}|_] = lists:reverse(lists:keysort(2, maps:to_list(Minutes))),
    Minute.

guard_sleep_schedule([], Minutes) -> Minutes;
guard_sleep_schedule([{_Awake, 'awake'} | Times], Minutes) ->
    guard_sleep_schedule(Times, Minutes);
guard_sleep_schedule([{Sleep, 'asleep'}
                     ,{Awake, 'awake'}
                      | Times
                     ]
                    ,Minutes
                    ) ->
    ElapsedMin = elapsed_minutes(Awake, Sleep),
    guard_sleep_schedule(Times, increment_minutes(Sleep, ElapsedMin, Minutes)).

elapsed_minutes(Sleep, Awake) ->
    to_minutes(Sleep) - to_minutes(Awake).

increment_minutes(_Timestamp, 0, Minutes) -> Minutes;
increment_minutes({_Y, _M, _D, _H, Min}=Time, ElapsedMin, Minutes) ->
    NextTime = next_minute(Time),
    increment_minutes(NextTime
                     ,ElapsedMin - 1
                     ,maps:update_with(Min, fun(X) -> X+1 end, 1, Minutes)
                     ).

next_minute({Year, Month, Day, 23, 59}) ->
    {Year, Month, Day+1, 0, 0};
next_minute({Year, Month, Day, Hour, 59}) ->
    {Year, Month, Day, Hour, 0};
next_minute({Year, Month, Day, Hour, Min}) ->
    {Year, Month, Day, Hour, Min+1}.

to_minutes({_Y, _M, D, H, Min}) ->
    Min + (H * 60) + (D * 24 * 60).

guard_times() ->
    {'ok', Bin} = file:read_file("p4.txt"),
    %% Bin = <<"[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up\n">>,
    process_guard_times(binary:split(Bin, <<"\n">>, ['global'])).

%% We can't know if this time is related to any guard (unless the guard is explicitly mentioned)
%% So we need to build the list by timestamp and status; further processing will need to add the implicit guard
process_guard_times(GuardTimes) ->
    Times = lists:foldl(fun process_guard_time/2, [], GuardTimes),
    associate_guard(lists:keysort(1, Times)).

associate_guard([{Start, 'start', Id} | Times]) ->
    associate_guard(Times, Id, [{Id, Start, 'awake'}]).

associate_guard([], _Id, OrderedTimes) ->
    lists:reverse(OrderedTimes);
associate_guard([{Start, 'start', NewId} | Times], _OldId, OrderedTimes) ->
    associate_guard(Times, NewId, [{NewId, Start, 'awake'} | OrderedTimes]);
associate_guard([{Time, Type, 'undefined'}|Times], Id, OrderedTimes) ->
    associate_guard(Times, Id, [{Id, Time, Type} | OrderedTimes]).

-define(TIMESTAMP(Year, Month, Day, Hour, Minute)
       ,"[", Year:4/binary, "-", Month:2/binary, "-", Day:2/binary
       ," ", Hour:2/binary, ":", Minute:2/binary, "] "
       ).

process_guard_time(<<>>, Acc) -> Acc;
process_guard_time(<<?TIMESTAMP(Year, Month, Day, Hour, Minute)
                   ,"Guard #", Rest/binary
                   >>
                  ,GuardTimes
                  ) ->
    {'match', [IdBin]} = re:run(Rest, <<"(\\d+) begins shift">>, [{'capture', 'all_but_first', 'binary'}]),
    Id = binary_to_integer(IdBin),
    [{{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)
      ,binary_to_integer(Hour), binary_to_integer(Minute)
      }
     ,'start'
     ,Id
     }
     | GuardTimes
    ];
process_guard_time(<<?TIMESTAMP(Year, Month, Day, Hour, Minute)
                   ,"falls asleep"
                   >>
                  ,GuardTimes
                  ) ->
    [{{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)
      ,binary_to_integer(Hour), binary_to_integer(Minute)
      }
     ,'asleep'
     ,'undefined'
     }
     | GuardTimes
    ];
process_guard_time(<<?TIMESTAMP(Year, Month, Day, Hour, Minute)
                   ,"wakes up"
                   >>
                  ,GuardTimes
                  ) ->
    [{{binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)
      ,binary_to_integer(Hour), binary_to_integer(Minute)
      }
     ,'awake'
     ,'undefined'
     }
     | GuardTimes
    ].
