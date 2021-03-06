-module(cycliq).

-export([archive/2
        ,stitch/0
        ]).

-include("cycliq.hrl").

archive(CameraType, Path) ->
    Clips = grab_clips(CameraType, Path),
    io:format("archived clips: ~p~n", [Clips]).

stitch() ->
    Clips = grab_clips_from_archive(),
    io:format("clips: ~p~n", [Clips]),
    %% grab listing of all files on memory card

    %% sort into probable rides
    Rides = sort_clips_into_rides(Clips),
    io:format("rides: ~p~n", [Rides]),

    %% ask ffmpeg to concat them into a single file
    _ = create_ride_videos(Rides),

    %% check if fly12 + fly6 videos exist for same date range
    %%   Picture-in-picture if so
    'ok'.

create_ride_videos(Rides) ->
    [create_ride_video(Ride) || Ride <- Rides].

create_ride_video(#ride{clips=Clips
                       ,start_time=Start
                       ,end_time=End
                       ,camera_type=CameraType
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
sort_clips_into_rides([]) -> [];
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

new_ride_from_clip(#clip{module=M}=Clip) ->
    #ride{start_time = start_seconds_from_clip(Clip)
         ,camera_type = atom_to_list(M)
         }.

add_clips_to_ride(Ride, [LastClip | _]=RideClips) ->
    Ride#ride{clips=lists:keysort(#clip.gregorian_seconds, RideClips)
             ,end_time=end_seconds_from_clip(LastClip)
             }.

%% 5 minutes between clip timestamps for same ride
clip_in_ride(#clip{module=M}=NextClip, PreviosClip) ->
    M:clip_seconds() >= (start_seconds_from_clip(NextClip) - start_seconds_from_clip(PreviosClip)).

start_seconds_from_clip(#clip{gregorian_seconds='undefined'}=Clip) ->
    calendar:datetime_to_gregorian_seconds(start_time_from_clip(Clip));
start_seconds_from_clip(#clip{gregorian_seconds=GS}) -> GS.

end_seconds_from_clip(#clip{gregorian_seconds='undefined'
                           ,module=M
                           }=Clip) ->
    calendar:datetime_to_gregorian_seconds(start_time_from_clip(Clip)) + M:clip_seconds();
end_seconds_from_clip(#clip{gregorian_seconds=GS
                           ,module=M
                           }) ->
    GS + M:clip_seconds().

start_time_from_clip(#clip{year=Y, month=Mo, day=D, hour=H, minute=M}) ->
    {{Y, Mo, D}, {H, M, 0}}.

-spec grab_clips(string(), file:filename_all()) -> [clip()].
grab_clips(CameraType, Path) ->
    Module = list_to_atom(CameraType),

    io:format("getting clips from ~p for ~p~n", [Path, CameraType]),
    {Module, Clips} = filelib:fold_files(Path
                                        ,".+\.[MP4|AVI]$"
                                        ,'true'
                                        ,fun grab_clip/2
                                        ,{Module, []}
                                        ),
    lists:keysort(#clip.gregorian_seconds, Clips).

grab_clips_from_archive() ->
    Clips = filelib:fold_files(clip_archive()
                              ,".*"
                              ,'true'
                              ,fun grab_archive_clip/2
                              ,[]
                              ),
    lists:keysort(#clip.archive_path, Clips).

grab_archive_clip(Filename, Acc) ->
    case archive_clip_meta(list_to_binary(filename:basename(Filename))) of
        'undefined' -> Acc;
        Clip ->
            [Clip#clip{archive_path=Filename
                      ,gregorian_seconds=start_seconds_from_clip(Clip)
                      }
             |Acc
            ]
    end.

archive_clip_meta(<<"fly12_", Year:4/binary, "-", Month:2/binary, "-", Day:2/binary
                    ,"_", Hour:2/binary, ":", Minute:2/binary
                    ,"_", Index:1/binary
                    ,_/binary
                  >>
                 ) ->
    Clip = #clip{year=binary_to_integer(Year)
                ,month=binary_to_integer(Month)
                ,day=binary_to_integer(Day)
                ,hour=binary_to_integer(Hour)
                ,minute=binary_to_integer(Minute)
                ,index=binary_to_integer(Index)
                ,module='fly12'
                },
    Clip#clip{gregorian_seconds=start_seconds_from_clip(Clip)};
archive_clip_meta(<<"fly6_", Year:4/binary, "-", Month:2/binary, "-", Day:2/binary
                    ,"_", Hour:2/binary, ":", Minute:2/binary
                    ,"_", Index:1/binary
                    ,_/binary
                  >>
                 ) ->
    Clip = #clip{year=binary_to_integer(Year)
                ,month=binary_to_integer(Month)
                ,day=binary_to_integer(Day)
                ,hour=binary_to_integer(Hour)
                ,minute=binary_to_integer(Minute)
                ,index=binary_to_integer(Index)
                ,module='fly6'
                },
    Clip#clip{gregorian_seconds=start_seconds_from_clip(Clip)};
archive_clip_meta(_Filename) ->
    io:format("invalid clip name '~s'~n", [_Filename]),
    'undefined'.

grab_clip(Filename, {Module, Acc}) ->
    io:format("grabbing ~s~n", [Filename]),
    case Module:grab_clip(Filename) of
        'undefined' -> {Module, Acc};
        Clip ->
            {Module, [Clip#clip{archive_path=archive_clip(Clip, Filename)
                               ,gregorian_seconds=start_seconds_from_clip(Clip)
                               }
                      | Acc
                     ]}
    end.

-spec archive_clip(clip(), file:filename_all()) -> file:filename_all().
archive_clip(Clip, Filename) ->
    ArchivedClipName = archive_clip_name(Clip),
    ArchivePath = filename:join([clip_archive(), ArchivedClipName]),

    'ok' = filelib:ensure_dir(ArchivePath),

    case filelib:is_regular(ArchivePath) of
        'true' -> io:format("  exists: ~s~n", [ArchivePath]);
        'false' ->
            io:format("  copying ~s to ~s~n", [Filename, ArchivePath]),
            {'ok', _} = file:copy(Filename, ArchivePath),
            'ok'
    end,
    ArchivePath.

archive_clip_name(#clip{year=Year
                       ,month=Month
                       ,day=Day
                       ,hour=Hour
                       ,minute=Minute
                       ,index=Index
                       ,module=Module
                       ,orig_path=OrigFilename
                       }
                 ) ->
    Ext = filename:extension(OrigFilename),
    iolist_to_binary(io_lib:format("~s_~w-~2..0w-~2..0w_~2..0w:~2..0w_~w~s"
                                  ,[Module, Year, Month, Day, Hour, Minute, Index, Ext]
                                  )).

clip_archive() ->
    filename:join([code:priv_dir('cycliq'), ?CLIP_PATH]).
