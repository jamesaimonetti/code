-module(cycliq).

-export([process/1]).

-include("cycliq.hrl").

process(CameraType) ->
    %% grab listing of all files on memory card
    Clips = grab_clips(CameraType),
    io:format("clips: ~p~n", [Clips]),

    %% sort into probable rides
    Rides = sort_clips_into_rides(Clips),
    io:format("rides: ~p~n", [Rides]),

    %% ask ffmpeg to concat them into a single file
    _ = create_ride_videos(CameraType, Rides),

    %% check if fly12 + fly6 videos exist for same date range
    %%   Picture-in-picture if so
    'ok'.

create_ride_videos(CameraType, Rides) ->
    [create_ride_video(CameraType, Ride) || Ride <- Rides].

create_ride_video(CameraType
                 ,#ride{clips=Clips
                       ,start_time=Start
                       ,end_time=End
                       }) ->
    VideoName = ride_video_name(CameraType, Start, End),
    case filelib:is_regular(VideoName) of
        'true' -> io:format("~s exists already~n", [VideoName]);
        'false' -> create_ride_video_from_clips(VideoName, Clips)
    end.

ride_video_name(CameraType, Start, End) ->
    {{Y, Mo, D}, {H, M, _}} = calendar:gregorian_seconds_to_datetime(Start),
    Name = iolist_to_binary(io_lib:format("~s_~w-~2..0w-~2..0w_~2..0w:~2..0w_~w.mp4"
                                         ,[CameraType, Y, Mo, D, H, M, End-Start]
                                         )),
    Path = filename:join([code:priv_dir('cycliq'), ?RIDE_PATH, Name]),

    'ok' = filelib:ensure_dir(Path),
    Path.

create_ride_video_from_clips(VideoName, Clips) ->
    Intermediates = create_intermediates(Clips),
    concat(VideoName, Intermediates),
    cleanup_intermediates(VideoName, Intermediates).

create_intermediates(Clips) ->
    [create_intermediate(Clip) || Clip <- Clips].

create_intermediate(#clip{archive_path=Input
                         ,gregorian_seconds=GS
                         }) ->
    Intermediate = filename:join([code:priv_dir('cycliq'), ?CLIP_PATH, integer_to_list(GS) ++ ".ts"]),

    case filelib:is_regular(Intermediate) of
        'true' -> 'ok';
        'false' ->
            Command = io_lib:format("ffmpeg -i ~s -c copy -bsf:v h264_mp4toannexb -f mpegts ~s"
                                   ,[Input, Intermediate]
                                   ),
            io:format("running: ~s~n", [Command]),
            _Ret = os:cmd(Command),
            io:format("and got: ~p~n", [_Ret])
    end,
    Intermediate.

concat(VideoName, Intermediates) ->
    Concat = string:join(Intermediates, "|"),
    Command = io_lib:format("ffmpeg -i \"concat:~s\" -c copy -bsf:a aac_adtstoasc ~s"
                           ,[Concat, VideoName]
                           ),
    io:format("concatting: ~s~n", [Command]),
    _Ret = os:cmd(Command),
    io:format("and got: ~p~n", [_Ret]),
    'ok'.

cleanup_intermediates(VideoName, Intermediates) ->
    case filelib:is_regular(VideoName) of
        'false' -> io:format("~s doesn't exist, not cleaning up~n", [VideoName]);
        'true' ->
            ['ok' = file:delete(I) || I <- Intermediates]
    end.

-spec sort_clips_into_rides([clip()]) -> [ride()].
sort_clips_into_rides([Clip | Clips]) ->
    Ride = new_ride_from_clip(Clip),

    sort_clips_into_rides(Clips, {Ride, [Clip]}, []).

sort_clips_into_rides([], {Ride, RideClips}, Rides) ->
    [add_clips_to_ride(Ride, RideClips)
     | Rides
    ];
sort_clips_into_rides([Clip | Clips], {Ride, [RideClip | _]=RideClips}, Rides) ->
    case clip_in_ride(Clip, RideClip) of
        'true' ->
            sort_clips_into_rides(Clips, {Ride, [Clip | RideClips]}, Rides);
        'false' ->
            NewRide = new_ride_from_clip(Clip),
            sort_clips_into_rides(Clips
                                 ,{NewRide, [Clip]}
                                 ,[add_clips_to_ride(Ride, RideClips) | Rides]
                                 )
    end.

new_ride_from_clip(Clip) ->
    #ride{start_time = start_seconds_from_clip(Clip)}.

add_clips_to_ride(Ride, [LastClip | _]=RideClips) ->
    Ride#ride{clips=lists:reverse(RideClips)
             ,end_time=end_seconds_from_clip(LastClip)
             }.

%% 5 minutes between clip timestamps for same ride
clip_in_ride(NextClip, PreviosClip) ->
    ?CLIP_SECONDS =:= (start_seconds_from_clip(NextClip) - start_seconds_from_clip(PreviosClip)).

start_seconds_from_clip(#clip{gregorian_seconds='undefined'}=Clip) ->
    calendar:datetime_to_gregorian_seconds(start_time_from_clip(Clip));
start_seconds_from_clip(#clip{gregorian_seconds=GS}) -> GS.

end_seconds_from_clip(#clip{gregorian_seconds='undefined'}=Clip) ->
    calendar:datetime_to_gregorian_seconds(start_time_from_clip(Clip)) + ?CLIP_SECONDS;
end_seconds_from_clip(#clip{gregorian_seconds=GS}) -> GS + ?CLIP_SECONDS.

start_time_from_clip(#clip{year=Y, month=Mo, day=D, hour=H, minute=M}) ->
    {{Y, Mo, D}, {H, M, 0}}.

-spec grab_clips(string()) -> [clip()].
grab_clips(CameraType) ->
    Module = list_to_atom(CameraType),

    {Module, Clips} = filelib:fold_files("/mnt/DCIM"
                                        ,"\\d+_\\d+\.MP4$"
                                        ,'true'
                                        ,fun grab_clip/2
                                        ,{Module, []}
                                        ),
    lists:keysort(#clip.orig_path, Clips).

grab_clip(Filename, {Module, Acc}) ->
    Clip = Module:grab_clip(Filename),
    {Module, [Clip#clip{archive_path=archive_clip(Filename)
                       ,gregorian_seconds=start_seconds_from_clip(Clip)
                       }
              | Acc
             ]}.

-spec archive_clip(file:filename_all()) -> file:filename_all().
archive_clip(Filename) ->
    Priv = code:priv_dir('cycliq'),
    Basename = filename:basename(Filename),
    ArchivePath = filename:join([Priv, ?CLIP_PATH, Basename]),

    'ok' = filelib:ensure_dir(ArchivePath),

    case filelib:is_regular(ArchivePath) of
        'true' -> 'ok';
        'false' ->
            io:format("copying ~s to ~s~n", [Filename, ArchivePath]),
            {'ok', _} = file:copy(Filename, ArchivePath),
            'ok'
    end,
    ArchivePath.
