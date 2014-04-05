-module(ws_handler_tests).
-include_lib("eunit/include/eunit.hrl").

init_should_immediatly_upgrade_to_a_websocket_test() ->
  Expected = {upgrade, protocol, cowboy_websocket},
  ?assertEqual( Expected, ws_handler:init( {tcp, http}, ignored, ignored) ).

websocket_init_returns_undefined_state_test() ->
  Expected = {ok, req1, undefined_state},
  ?assertEqual( Expected, ws_handler:websocket_init( transport1, req1, opts1 ) ).

websocket_handle_should_echo_received_text_test() ->
  Message = message1,
  Expected = {reply, {text, Message}, req1, state1 },
  Actual = ws_handler:websocket_handle({text, Message}, req1, state1),
  ?assertEqual( Expected, Actual ).

websocket_handle_should_take_no_action_on_non_text_messages_test() ->
  Expected = {ok, req1, state1},
  Actual = ws_handler:websocket_handle({wtf, mate}, req1, state1),
  ?assertEqual( Expected, Actual ).

websocket_info_should_send_nothing_on_timeout_test() ->
  Expected = {ok, req1, state1},
  Actual = ws_handler:websocket_info( {timeout, ref1, msg1}, req1, state1),
  ?assertEqual( Expected, Actual ).

websocket_terminate_should_return_ok_test() ->
  ?assertEqual( ok, ws_handler:websocket_terminate( reason1, req1, state1 ) ).