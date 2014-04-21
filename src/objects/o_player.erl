-module(o_player).

-export([sprite/0]).

sprite() ->
  #{ type => ?MODULE, bank => human, state => ghost }.