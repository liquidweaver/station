-module(ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("sample_world_data.hrl").
-include("records.hrl").

init_should_immediatly_upgrade_to_a_websocket_test() ->
  Expected = {upgrade, protocol, cowboy_websocket},
  ?assertEqual( Expected, ws_handler:init( {tcp, http}, ignored, ignored) ).

websocket_init_returns_new_client_state_state_test() ->
  Expected = {ok, req1, #client_state{} },
  ?assertEqual( Expected, ws_handler:websocket_init( transport1, req1, opts1 ) ).

websocket_handle_when_no_user_and_username_sent_should_set_username_and_send_world_data_test() ->
  Message = jiffy:encode( {[{<<"username">>, <<"bob">> }]} ),
  State = #client_state{},
  State1 = #client_state{ username = <<"bob">> },
  Expected = {reply, {text, ?SAMPLE_WORLD_DATA}, req1, State1 },
  Actual = ws_handler:websocket_handle({text, Message}, req1, State),
  ?assertEqual( Expected, Actual ).

websocket_handle_when_no_user_should_send_need_login_test() ->
  Message = <<"{}">>,
  State = #client_state{},
  Expected = {reply, {text, jiffy:encode( {[{ <<"need_login">>, <<"Please pass your username.">>}]} )}, req1, State },
  Actual = ws_handler:websocket_handle({text, Message}, req1, State),
  ?assertEqual( Expected, Actual ).

websocket_handle_when_user_should_send_sample_world_data_test() ->
  Message = message1,
  State = #client_state{ username = bob },
  Expected = {reply, {text, ?SAMPLE_WORLD_DATA}, req1, State },
  Actual = ws_handler:websocket_handle({text, Message}, req1, State),
  ?assertEqual( Expected, Actual ).

websocket_handle_should_take_no_action_on_non_text_messages_test() ->
  Expected = {ok, req1, state1},
  Actual = ws_handler:websocket_handle({wtf, mate}, req1, state1),
  ?assertEqual( Expected, Actual ).

websocket_info_need_login_should_send_login_request_test() ->
  Expected = {reply, {text, jiffy:encode( {[{ <<"need_login">>, <<"Please pass your username.">>}]} )}, req1, state1 },
  ?assertEqual( Expected, ws_handler:websocket_info( need_login, req1, state1 )).

websocket_terminate_should_return_ok_test() ->
  ?assertEqual( ok, ws_handler:websocket_terminate( reason1, req1, state1 ) ).