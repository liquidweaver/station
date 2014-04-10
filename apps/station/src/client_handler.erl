-module(client_handler).
-include("sample_world_data.hrl").
-include("records.hrl").

-export([request/3]).

request( <<"username">>, Username, State = #client_state{}) when 
  is_binary(Username) andalso byte_size(Username) > 0 ->
  { objects([
    {tile_data, ?SAMPLE_WORLD_DATA},
    {world_pos, objects([
      {x, State#client_state.x},
      {y, State#client_state.y}
    ])}
    ]), State#client_state{ username = Username } };

request( <<"username">>, _, State) ->
  { object(error, <<"Invalid username.">>), State };

request( _, _, State = #client_state{ username = undefined }) ->
  { object(need_login, <<"Please pass your username.">>), State};

request( <<"send_tiles">>, _, State) ->
  { objects([
    {tile_data, ?SAMPLE_WORLD_DATA},
    {world_pos, objects([
      {x, State#client_state.x},
      {y, State#client_state.y}
    ])}
    ]), State };

request( <<"move_intent">>, <<"right">>, State = #client_state{ x = X, y = Y} ) ->
  X1 = X + 1,
  { object( world_pos, objects([{x, X1}, {y, Y} ]) ), State#client_state{ x = X1 } };

request( <<"move_intent">>, <<"left">>, State = #client_state{ x = X, y = Y} ) ->
  X1 = X - 1,
  { object( world_pos, objects([{x, X1}, {y, Y} ]) ), State#client_state{ x = X1 } };

request( <<"move_intent">>, <<"down">>, State = #client_state{ x = X, y = Y} ) ->
  Y1 = Y + 1,
  { object( world_pos, objects([{x, X}, {y, Y1} ]) ), State#client_state{ y = Y1 } };

request( <<"move_intent">>, <<"up">>, State = #client_state{ x = X, y = Y} ) ->
  Y1 = Y - 1,
  { object( world_pos, objects([{x, X}, {y, Y1} ]) ), State#client_state{ y = Y1 } };

request( Unknown, Data, State) ->
  {  {[{unknown_request, {[{ Unknown, Data }]} }]}, State }.


objects( Objects ) when is_list(Objects) ->
  { Objects }.

object( Key, Value ) ->
  {[{ Key, Value }]}.

