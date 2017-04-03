#!/usr/bin/env escript
%%! +A0 -sname kazoo_src2any -pa ebin -pa deps/getopt/ebin
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main([]) -> help();
main(Args) ->
    {'ok', {Options}} = getopt:parse(help_list(), Args),
    process_request(Options).

help() ->
    getopt:usage(help_list(), escript:script_name()),
    halt(0).

process_request(Options) ->
    maybe_help(Options),

    maybe_archive(Options),

    maybe_process(Options),

    maybe_list(Options).

maybe_help(Options) ->
    case opt_help(Options) of
        'false' -> 'ok';
        'true' -> help()
    end.

maybe_archive(Options) ->
    case opt_archive(Options) of
        'false' -> 'ok';
        Path ->
            CameraType = opt_camera_type(Options),
            cycliq:archive(CameraType, Path)
    end.

maybe_process(Options) ->
    case opt_stitch(Options) of
        'false' -> 'ok';
        'true' -> cycliq:stitch()
    end.

maybe_list(Options) ->
    maybe_list_clips(Options) orelse maybe_list_rides(Options).

maybe_list_clips(Options) ->
    opt_list_clips(Options)
        andalso begin cycliq:list_clips(), 'true' end.

maybe_list_rides(Options) ->
    opt_list_rides(Options) andalso cycliq:list_rides().

opt_list_clips(Options) ->
    proplists:get_value('list_clips', Options, 'false').

opt_list_rides(Options) ->
    proplists:get_value('list_rides', Options, 'false').

opt_help(Options) ->
    proplists:get_value('help', Options, 'false').

opt_archive(Options) ->
    lists:keyfind('archive', 1, Options).

opt_camera_type(Options) ->
    case lists:keyfind('camera', 1, Options) of
        'false' -> "fly12";
        "fly12"=CameraType -> CameraType;
        "fly6"=CameraType -> CameraType;
        _Type -> help()
    end.

opt_stitch(Options) ->
    proplists:get_value('stitch', Options).

help_list() ->
    [{'archive',    $a,            "archive",    {'string', "/mnt"},  "Archive files from the memory card first"}
    ,{'stitch',     $s,            "stitch",     'undefined',         "Stitch together rides from clips"}
    ,{'camera',     $c,            "camera",     {'string', "fly12"}, "Camera the files are being archived from (fly12 or fly6)"}
    ,{'list_clips', 'undefined',   "list-clips", 'undefined',         "List archived clips"}
    ,{'list_rides', 'undefined',   "list-rides", 'undefined',         "List archived rides"}
    ,{'help',       $h,            "help",       'undefined',         "Display this help"}
    ].
