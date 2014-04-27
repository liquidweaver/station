-module(game_time).

-export([timestamp/0, tick_length/1]).

timestamp() ->
  {Mega,Sec,Micro} = erlang:now(),
  (Mega * 1000000 + Sec) * 1000 + Micro div 1000.

tick_length( Count ) -> Count * 150.