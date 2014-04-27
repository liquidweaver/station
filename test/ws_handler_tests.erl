-module(ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

init_should_immediately_upgrade_to_a_websocket_test() ->
  Expected = {upgrade, protocol, cowboy_websocket},
  ?assertEqual( Expected, ws_handler:init( {tcp, http}, ignored, ignored) ).

websocket_init_returns_new_client_state_state_test() ->
  Expected = {ok, req1, #{ username => undefined } },
  ?assertEqual( Expected, ws_handler:websocket_init( transport1, req1, opts1 ) ).

websocket_terminate_should_call_client_handler_remove_player_and_return_result_test() ->
  meck:new(client_handler),
  Expected = result1,
  meck:expect(client_handler, remove_player_from_world, 1, Expected),

  Actual = ws_handler:websocket_terminate( reason1, req1, state1 ),

  ?assertEqual ( Expected, Actual ),
  ?assert( meck:called(client_handler, remove_player_from_world, [state1]) ),
  meck:unload(client_handler).

websocket_handle_delegates_text_requests_to_client_handler_request_test() ->
  meck:new(client_handler),
  meck:expect( client_handler, request, 3, {replydata1, state1} ),
  meck:new(map_codec),
  meck:expect( map_codec, encode, 1, ignored ),

  ws_handler:websocket_handle({text, <<"{ \"type\":\"data\" }">>}, req1, state1 ),

  ?assert( meck:validate(client_handler)),
  ?assert( meck:called(client_handler, request, [<<"type">>, <<"data">>, state1] ) ),
  meck:unload(client_handler),
  meck:unload(map_codec).

websocket_should_take_no_action_on_non_text_messages_test() ->
  Expected = {ok, req1, state1},
  Actual = ws_handler:websocket_handle({wtf, mate}, req1, state1),
  ?assertEqual( Expected, Actual ).

websocket_info_need_login_should_send_login_request_test() ->
  Expected = {reply, {text, map_codec:encode(#{ need_login => <<"Please pass your username.">>})}, req1, state1 },
  ?assertEqual( Expected, ws_handler:websocket_info( need_login, req1, state1 )).

websocket_info_tile_data_should_return_tile_data_test() ->
  SpriteData = #{ type => o_space, bank => space, state => 0},
  Expected = map_codec:encode( #{tile_data => #{ <<"0,0">> => SpriteData} } ),
  {reply, {text, Actual}, ignored, also_ignored} = ws_handler:websocket_info( {tile_data, {0,0}, SpriteData }, ignored, also_ignored ),
  ?assertEqual( Expected, Actual ).
