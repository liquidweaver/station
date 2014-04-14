-module(tile_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

init_passed_contents_sets_contents_in_state_test() ->
  Actual = tile:init( {x,y,contents1} ),
  ?assertMatch( {ok, #tile_state{ contents = contents1 } } , Actual).

handle_cast_add_object_pushes_new_object_onto_contents_test() ->
  Expected = [old_object, new_object],
  State = #tile_state{ contents = [old_object] },
  {noreply, #tile_state{ contents = Actual} } = tile:handle_cast({add_object, new_object}, State),
  ?assertEqual( Expected, Actual ).