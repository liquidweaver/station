-module(o_player).

-export([ new/1, sprite/1, moved/2, blocks/2]).

new( #{ name := Name } ) ->
  #{ type => ?MODULE, direction => south, name => Name }.

sprite( #{ direction := Direction } ) ->
  #{ type => ?MODULE, bank => human, state => fatbody_s, direction => Direction}.

moved( {From, To}, State = #{ direction := OldDirection } ) ->
  NewDirection = direction_from_coords( From, To, OldDirection ),
  State#{ direction => NewDirection }.

blocks(_Other, Self) ->
  {true, Self}.

direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX > OldX -> east;
direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX < OldX -> west;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY > OldY -> south;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY < OldY -> north;
direction_from_coords( _, _, OldDirection ) -> OldDirection.