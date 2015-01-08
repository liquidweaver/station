-module( a_pickup ).
-behavior(b_action).
-export([do_action/2]).

do_action( Player = #{ type := o_player, active_hand := ActiveHand, left_hand := LeftHand, right_hand := RightHand }, Object ) ->
  Player1 = case { ActiveHand, LeftHand, RightHand } of
    {right, _, empty} -> Player#{ right_hand := Object };
    {left, empty, _} -> Player#{ left_hand := Object };
    {_, _, _} -> Player %% XXX Maybe side effect of message to player indicating hand is full
  end,
  { Player1, deleted, []}.
