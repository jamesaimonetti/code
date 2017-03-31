#!/usr/bin/env escript
%%! +A0 -sname kazoo_src2any -pa ebin
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% CameraType = ("fly12", "fly6")
main([]) -> main(["fly12"]);
main([CameraType]) ->
    cycliq:process(CameraType).
