-module(o_floor).

-export([ new/2, sprite/1, moved/2, blocks/2]).

new(_,_) ->
  #{ type => ?MODULE }.

sprite(_) ->
  #{ type => ?MODULE, bank => floors, state => floor }.

moved({_From, _To}, State ) ->
  State.

blocks(_Other, Self) ->
  {false, Self}.