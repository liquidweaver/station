-module(o_player).

-export([sprite/0]).

sprite() ->
  { ?MODULE, human, ghost }.