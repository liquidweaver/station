-module(client_handler_tests).
-include_lib("eunit/include/eunit.hrl").

-include("sample_world_data.hrl").
-include("records.hrl").

request_when_no_user_and_username_sent_should_set_username_and_send_world_data_test() ->
  State = #client_state{},
  State1 = #client_state{ username = <<"bob">> },
  Expected = {tile_data, ?SAMPLE_WORLD_DATA, State1 },
  Actual = client_handler:request(<<"username">>, <<"bob">>, State),
  ?assertEqual( Expected, Actual ).

request_when_no_user_should_send_need_login_test() ->
  State = #client_state{},
  Expected = {need_login, <<"Please pass your username.">>, State },
  Actual = client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

request_when_username_empty_should_send_error_test() ->
  State = #client_state{},
  Expected = {error, <<"Invalid username.">>, State },
  Actual = client_handler:request(<<"username">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

request_when_request_tiles_should_send_sample_world_data_test() ->
  State = #client_state{ username = bob },
  Expected = {tile_data, ?SAMPLE_WORLD_DATA, State },
  Actual = client_handler:request(<<"send_tiles">>, <<>>, State),
  ?assertEqual( Expected, Actual ).

json_request( Type, Message ) ->
  jiffy:encode( {[{ Type, Message }]}).

all_tile_data() ->
  json_request( tile_data, ?SAMPLE_WORLD_DATA ).