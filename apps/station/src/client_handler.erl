-module(client_handler).
-include("sample_world_data.hrl").
-include("records.hrl").

-export([request/3]).

request( <<"username">>, Username, State = #client_state{}) when 
  is_binary(Username) andalso byte_size(Username) > 0 ->
  { {[{tile_data, ?SAMPLE_WORLD_DATA},{world_pos, {[{x, State#client_state.x}, {y, State#client_state.y}]}}]}, State#client_state{ username = Username } };

request( <<"username">>, _, State) ->
  { {[{error, <<"Invalid username.">>}]}, State };

request( _, _, State = #client_state{ username = undefined }) ->
  { {[{need_login, <<"Please pass your username.">>}]}, State};

request( <<"send_tiles">>, _, State) ->
  { {[{tile_data, ?SAMPLE_WORLD_DATA},{world_pos, {[{x, State#client_state.x}, {y, State#client_state.y}]}}]}, State };

request( Unknown, Data, State) ->
  {  {[{unknown_request, {[{ Unknown, Data }]} }]}, State }.
