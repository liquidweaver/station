-module(o_cryosleeper).

-export([sprite/1, new/1]).

sprite( #{ direction := Direction } ) ->
  #{ type => ?MODULE, bank => cryogenic2, state => sleeper_1, start => 0, direction => Direction }.

new(_) ->
  #{ type => ?MODULE, direction => south }.
