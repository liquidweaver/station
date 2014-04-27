-module(tile_tests).
-include_lib("eunit/include/eunit.hrl").

init_passed_contents_sets_contents_in_state_test() ->
  Actual = tile:init( {x,y,contents1} ),
  ?assertMatch( {ok, #{ contents := contents1 } } , Actual).

handle_cast_add_object_pushes_new_object_onto_contents_test() ->
  Expected = [old_object, new_object],
  State = #{ contents => [old_object], tile_subscribers => sets:new(), x => ignored, y => ignored },
  {noreply, #{ contents := Actual} } = tile:handle_cast({add_object, new_object}, State),
  ?assertEqual( Expected, Actual ).

handle_call_remove_object_removes_object_from_contents_test() ->
  State = #{ contents => [object1, object2], tile_subscribers => sets:new(), x => ignored, y => ignored },
  Actual = tile:handle_call( {remove_object, object2}, ignored, State ),

  ?assertMatch( {_, _, #{ contents := [object1] } }, Actual ).

handle_cast_remove_tile_subscription_removes_supplied_pid_from_tile_subscribers_test() ->
  State = #{ tile_subscribers => sets:from_list( [ somepid, otherpid ] ) },
  Expected = sets:from_list([otherpid]),
  {noreply, #{ tile_subscribers := Actual } } = tile:handle_cast( { remove_tile_subscription, somepid }, State ),
  ?assertEqual( Expected, Actual ).