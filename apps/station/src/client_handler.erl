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

request( Unknown, Data, State) ->
  {  {[{unknown_request, {[{ Unknown, Data }]} }]}, State }.


objects( Objects ) when is_list(Objects) ->
  { Objects }.

object( Key, Value ) ->
  {[{ Key, Value }]}.

