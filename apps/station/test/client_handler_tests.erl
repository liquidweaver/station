-module(client_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sample_world_data.hrl").
-include("records.hrl").

request_when_no_user_and_username_sent_should_set_username_and_send_world_data_test() ->
  State = #client_state{},
  State1 = #client_state{ username = <<"bob">> },
  Expected = all_tile_data(State1),
  Actual = client_handler:request(<<"username">>, <<"bob">>, State),
  ?assertEqual( Expected, Actual ).

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

request_when_send_tiles_should_send_sample_world_data_test() ->
  State = #client_state{ username = bob },
  Expected = all_tile_data(State),
  Actual = client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

request_when_move_intent_right_increments_client_state_x_test() ->
  State = #client_state{ x=7, y=ignore, username=ignore },
  State1 = State#client_state{ x=8 },
  Expected = { {[{world_pos, {[{ x, State1#client_state.x}, {y, State1#client_state.y}]}}]}, State1},
  Actual = client_handler:request( <<"move_intent">>, <<"right">>, State ),
  ?assertEqual( Expected, Actual ).

request_when_move_intent_left_decrements_client_state_x_test() ->
  State = #client_state{ x=7, y=ignore, username=ignore },
  State1 = State#client_state{ x=6 },
  Expected = { {[{world_pos, {[{ x, State1#client_state.x}, {y, State1#client_state.y}]}}]}, State1},
  Actual = client_handler:request( <<"move_intent">>, <<"left">>, State ),
  ?assertEqual( Expected, Actual ).

request_when_move_intent_down_increments_client_state_y_test() ->
  State = #client_state{ x=ignore, y=7, username=ignore },
  State1 = State#client_state{ y=8 },
  Expected = { {[{world_pos, {[{ x, State1#client_state.x}, {y, State1#client_state.y}]}}]}, State1},
  Actual = client_handler:request( <<"move_intent">>, <<"down">>, State ),
  ?assertEqual( Expected, Actual ).

request_when_move_intent_up_decrements_client_state_y_test() ->
  State = #client_state{ x=ignore, y=7, username=ignore },
  State1 = State#client_state{ y=6 },
  Expected = { {[{world_pos, {[{ x, State1#client_state.x}, {y, State1#client_state.y}]}}]}, State1},
  Actual = client_handler:request( <<"move_intent">>, <<"up">>, State ),
  ?assertEqual( Expected, Actual ).

all_tile_data(State) ->
  { {[{tile_data, ?SAMPLE_WORLD_DATA}, {world_pos, {[{ x, State#client_state.x}, {y, State#client_state.y}]}}]}, State}.