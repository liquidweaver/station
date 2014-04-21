-module(o_cryosleeper).

-export([sprite/0]).

sprite() ->
  #{ type => ?MODULE, bank => cryogenic2, state => sleeper_1, start => 0 }.
