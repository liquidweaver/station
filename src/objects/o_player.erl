-module(o_player).

-export([sprite/1, moving/2, new/1]).

sprite( #{ direction := Direction } ) ->
  #{ type => ?MODULE, bank => human, state => fatbody_s, direction => Direction}.

moving( {From, To}, State = #{ direction := OldDirection } ) ->
  NewDirection = direction_from_coords( From, To, OldDirection ),
  State#{ direction => NewDirection }.

new( #{ name := Name } ) ->
  #{ type => ?MODULE, direction => south, name => Name }.

direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX > OldX -> east;
direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX < OldX -> west;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY > OldY -> south;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY < OldY -> north;
direction_from_coords( _, _, OldDirection ) -> OldDirection.