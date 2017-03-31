

%% each clip is up to 5 minutes long
-define(CLIP_PATH, "clips").
-define(RIDE_PATH, "rides").

-type gregorian_seconds() :: pos_integer().

-record(clip, {orig_path :: file:filename_all() | 'undefined'
              ,archive_path :: file:filename_all() | 'undefined'
              ,year :: 2017..2020
              ,month :: 1..12
              ,day :: 1..31
              ,hour :: 0..23
              ,minute :: 0..59
              ,index :: pos_integer() | 'undefined'
              ,gregorian_seconds :: gregorian_seconds() | 'undefined'
              ,module :: atom()
              }).
-type clip() :: #clip{}.

-record(ride, {clips = [] :: [clip()]
              ,start_time :: gregorian_seconds() | 'undefined'
              ,end_time :: gregorian_seconds() | 'undefined'
              }).
-type ride() :: #ride{}.
