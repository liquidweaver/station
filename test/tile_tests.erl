-module(tile_tests).
-include_lib("eunit/include/eunit.hrl").

init_passed_contents_sets_contents_in_state_test() ->
  Actual = tile:init( {x,y,contents1} ),
  ?assertMatch( {ok, #{ contents := contents1 } } , Actual).

handle_cast_add_object_pushes_new_object_onto_contents_test() ->
  Expected = [old_object, new_object],
  State = #{ contents => [old_object] },
  {noreply, #{ contents := Actual} } = tile:handle_cast({add_object, new_object}, State),
  ?assertEqual( Expected, Actual ).

handle_call_remove_object_removes_object_from_contents_test() ->
  State = #{ contents => [object1, object2] },
  Actual = tile:handle_call( {remove_object, object2}, ignored, State ),

  ?assertMatch( {_, _, #{ contents := [object1] } }, Actual ).