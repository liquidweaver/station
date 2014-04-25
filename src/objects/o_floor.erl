-module(o_floor).

-export([ new/1, sprite/1, moved/2, blocks/2]).

new(_) ->
  #{ type => ?MODULE }.

sprite(_) ->
  #{ type => ?MODULE, bank => floors, state => floor }.

moved({_From, _To}, State ) ->
  State.

blocks(_Other, Self) ->
  {false, Self}.