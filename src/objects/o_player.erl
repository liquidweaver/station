-module(o_player).

-export([ new/2, sprite/1, moved/2, blocks/2, actions/1]).

new( _, State ) ->
  State#{ direction => south, left_hand => empty, right_hand => empty, active_hand => left }.

sprite( #{ direction := Direction } ) ->
  #{ type => ?MODULE, bank => human, state => fatbody_s, direction => Direction}.

moved( {From, To}, State = #{ direction := OldDirection } ) ->
  NewDirection = direction_from_coords( From, To, OldDirection ),
  State#{ direction => NewDirection }.

blocks(_Other, Self) ->
  {true, Self}.

actions( #{ active_hand := left, left_hand := LeftHand } ) ->
  case LeftHand of
    empty ->
      [pickup];
    _Object ->
      [place]
  end;
actions( #{ active_hand := right, right_hand := RightHand } ) ->
    case RightHand of
    empty ->
      [pickup];
    _Object ->
      [place]
  end.

direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX > OldX -> east;
direction_from_coords( {OldX, _}, {NewX, _}, _OldDirection ) when NewX < OldX -> west;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY > OldY -> south;
direction_from_coords( {_, OldY}, {_, NewY}, _OldDirection ) when NewY < OldY -> north;
direction_from_coords( _, _, OldDirection ) -> OldDirection.