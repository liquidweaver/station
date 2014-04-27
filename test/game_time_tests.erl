-module(game_time_tests).
-include_lib("eunit/include/eunit.hrl").

timestamp_returns_milliseconds_from_epoch_test() ->
  {Mega,Sec,Micro} = erlang:now(),
  Expected = (Mega*1000000+Sec)*1000+ Micro div 1000,
  Actual = game_time:timestamp(),
  ?assert( Actual - Expected < 5000 ). % Close enough

tick_length_returns_milliseconds_for_tick_count_test() ->
  Expected = 300,
  Actual = game_time:tick_length(2),
  ?assertEqual( Expected, Actual).
