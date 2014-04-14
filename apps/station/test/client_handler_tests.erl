-module(client_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-include("records.hrl").

remove_player_from_world_calls_tile_remove_object_test() ->
  meck:new( tile ),
  meck:expect( tile, remove_object, 2, ok),
  State = #client_state{ x = x1, y = y1 },
  client_handler:remove_player_from_world( State),

  ?assert( meck:called( tile, remove_object, [{x1,y1}, #thing{ type = o_player }]) ),
  meck:unload( tile ). 

request_when_no_user_and_username_sent_should_set_username_test() ->
  meck:new( tile, [pass_through]),
  meck:expect( tile, sprites, 1, [{bank1, state1}] ),
  meck:expect( tile, add_object, 2, ok), %%% WHY ISN'T THIS FAILING
  State = #client_state{},
  Expected = <<"bob">>,
  {_,#client_state{ username = Actual } } = client_handler:request(<<"username">>, Expected, State),
  ?assertEqual( Expected, Actual ),
  ?assert( meck:validate( tile ) ),
  ?assert( meck:called( tile, add_object, ['_', #thing{type = o_player}])),
  meck:unload(tile).

request_when_no_user_should_send_need_login_test() ->
  State = #client_state{},
  Expected = { {[{need_login, <<"Please pass your username.">>}]}, State },
  Actual = client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

request_when_username_empty_should_send_error_test() ->
  State = #client_state{},
  Expected = { {[{error, <<"Invalid username.">>}]}, State },
  Actual = client_handler:request(<<"username">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

request_when_send_tiles_should_delegate_call_to_tiles_sprites_test() ->
  meck:new( tile ),
  meck:expect( tile, sprites, 1, [{bank1, state1}] ),
  State = #client_state{ x = 7, y = 7, username = ignored },
  client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assert( meck:validate( tile ) ),
  meck:unload(tile).

request_move_intent_test_() ->
  {foreach,
  fun() -> 
    meck:new(tile)
  end,
  fun(_) ->
    meck:unload(tile)
  end,
  [{"right_increments_client_state_x",
    fun() ->
      meck:expect( tile, sprites, 1, [{bank1, state1}] ),
      meck:expect( tile, move_object, 3, ok ),
      State = #client_state{ x=7, y=7, username=ignore },
      Actual = client_handler:request( <<"move_intent">>, <<"right">>, State ),
      ?assertMatch( {_,#client_state{ x=8 }}, Actual )
    end},

  {"left_decrements_client_state_x",
  fun() ->
    meck:expect( tile, sprites, 1, [{bank1, state1}] ),
    meck:expect( tile, move_object, 3, ok ),
    State = #client_state{ x=7, y=7, username=ignore },
    Actual = client_handler:request( <<"move_intent">>, <<"left">>, State ),
    ?assertMatch( {_,#client_state{ x=6 }}, Actual )
  end},

  {"down_increments_client_state_y",
  fun() ->
    meck:expect( tile, sprites, 1, [{bank1, state1}] ),
    meck:expect( tile, move_object, 3, ok ),
    State = #client_state{ x=7, y=7, username=ignore },
    Actual = client_handler:request( <<"move_intent">>, <<"down">>, State ),
    ?assertMatch( {_, #client_state{ y=8 }}, Actual )
  end},

  {"up_decrements_client_state_y",
  fun() ->
    meck:expect( tile, sprites, 1, [{bank1, state1}] ),
    meck:expect( tile, move_object, 3, ok ),
    State = #client_state{ x=7, y=7, username=ignore },
    Actual = client_handler:request( <<"move_intent">>, <<"up">>, State ),
    ?assertMatch( {_,#client_state{ y=6 }}, Actual )
  end},

  {"no_movement_when_tile_transfer_of_player_fails",
  fun() ->
    meck:expect( tile, sprites, 1, [{bank1, state1}] ),
    meck:expect( tile, move_object, 3, {error, blocked} ),

    State = #client_state{ x=7, y=7, username=ignore },
    Result = client_handler:request( <<"move_intent">>, <<"up">>, State ),
    ?assert( meck:validate( tile ) ),
    ?assertMatch( {_, #client_state{ x=7, y=7 } }, Result )
  end}
  ] }.



