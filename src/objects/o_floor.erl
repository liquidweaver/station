-module(o_floor).

-export([ new/2, sprite/1, moved/2, blocks/2, actions/1]).

new(_,State) ->
  State.

sprite(_) ->
  #{ type => ?MODULE, bank => floors, state => floor }.

moved({_From, _To}, State ) ->
  State.


actions(_) -> [place].

blocks(_Other, Self) ->
  {false, Self}.