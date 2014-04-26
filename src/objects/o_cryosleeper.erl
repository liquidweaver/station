-module(o_cryosleeper).

-export([ new/2, sprite/2, moved/2, blocks/2]).

new(_,_) ->
  #{ type => ?MODULE, direction => south }.

sprite( _,#{ direction := Direction } ) ->
  #{ type => ?MODULE, bank => cryogenic2, state => sleeper_1, start => 0, direction => Direction }.

moved({_From, _To}, State ) ->
  State.

blocks(_Other, Self) ->
  {true, Self}.