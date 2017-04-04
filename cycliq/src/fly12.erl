-module(fly12).

-export([grab_clip/1
        ,clip_seconds/0
        ]).

-include("cycliq.hrl").

clip_seconds() -> 300. %% 5 minutes


grab_clip(Filename) ->
    clip_metadata(Filename).

clip_metadata(Filename) ->
    case base_data(filename:basename(Filename, ".MP4")) of
        'undefined' -> 'undefined';
        BaseClip ->
            [_, _, _, $_ | Year] = filename:basename(filename:dirname(Filename)),

            BaseClip#clip{orig_path=Filename
                         ,year=year(Year)
                         ,module=?MODULE
                         }
    end.

base_data([_|_]=Basename) ->
    base_data(list_to_binary(Basename));
base_data(<<Mo:2/binary
            ,Day:2/binary
            ,Hour:2/binary
            ,Minute:2/binary
            ,"_"
            ,Index:4/binary
          >>
         ) ->
    #clip{month=binary_to_integer(Mo)
         ,day=binary_to_integer(Day)
         ,hour=binary_to_integer(Hour)
         ,minute=binary_to_integer(Minute)
         ,index=binary_to_integer(Index)
         };
base_data(_Basename) ->
    io:format("skipping base name ~s~n", [_Basename]),
    'undefined'.


year([_, _, _, _] = YearList) ->
    list_to_integer(YearList).
