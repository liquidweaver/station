-module(o_beaker).
-behavior(b_object).

-export([ new/2, sprite/1, moved/2, blocks/2, actions/1]).

new(_,State) ->
  State.

sprite(_) ->
  #{ type => ?MODULE, bank => chemical, state => beakerlarge }.

moved({_From, _To}, State ) ->
  State.

blocks(_Other, Self) ->
  {false, Self}.

actions(_) -> [pickup].