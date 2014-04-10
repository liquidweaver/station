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

request_when_request_tiles_should_send_sample_world_data_test() ->
  State = #client_state{ username = bob },
  Expected = all_tile_data(State),
  Actual = client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

all_tile_data(State) ->
  { {[{tile_data, ?SAMPLE_WORLD_DATA}, {world_pos, {[{ x, State#client_state.x}, {y, State#client_state.y}]}}]}, State}.