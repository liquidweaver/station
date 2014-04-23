-module(o_door).

-export([ new/1, sprite/1, moving/2, blocks/2]).

new(_) ->
  #{ type => ?MODULE, state => door1 }.

sprite( #{ state := State } ) ->
  #{ type => ?MODULE, bank => 'Door1', state => State, start => 0 }.

moving({_From, _To}, State ) ->
  State.

blocks(_Other, Self) ->
  {true, Self#{state => door_deny}}.