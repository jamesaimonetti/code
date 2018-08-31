#!/usr/bin/env escript
%%! +A0 -sname dbus_slack
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

-define(INTERFACE, <<"org.freedesktop.Notifications">>).
-define(PATH, <<"/org/freedesktop/Notifications">>).
-define(MSG_SIGNAL, <<"ActionInvoked">>).
-define(MSG_CLOSED, <<"NotificationClosed">>).

main(_) ->
    {'ok', Bus} = dbus_bus_connection:connect(session),
    {'ok', Proxy} = dbus_proxy:start_link(Bus, ?INTERFACE, ?PATH),
    'ok' = dbus_proxy:connect_signal(Proxy, ?INTERFACE, ?MSG_SIGNAL, self()),
    'ok' = dbus_proxy:connect_signal(Proxy, ?INTERFACE, ?MSG_CLOSED, self()),

    'ok' = dbus_proxy:connect_signal(Proxy, self()),

    loop().

loop() ->
    receive
        Msg ->
            io:format("msg: ~p~n", [Msg]),
            loop()
    end.
