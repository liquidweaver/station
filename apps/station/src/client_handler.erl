-module(client_handler).
-include("records.hrl").

-export([request/3]).

-export([objects/1, object/2, view/3, remove_player_from_world/1]). %debugging

request( <<"username">>, Username, State = #client_state{ x = X, y = Y}) when 
  is_binary(Username) andalso byte_size(Username) > 0 ->

  LoggedInState = login(Username, State),
  WorldData = objects(view(X,Y,15)),
  { objects([
    {tile_data, WorldData},
    {world_pos, objects([
      {x, LoggedInState#client_state.x},
      {y, LoggedInState#client_state.y}
    ])}
    ]), LoggedInState };

request( <<"username">>, _, State) ->
  { object(error, <<"Invalid username.">>), State };

request( _, _, State = #client_state{ username = undefined }) ->
  { object(need_login, <<"Please pass your username.">>), State};

request( <<"send_tiles">>, _, State) ->
  {world_pos_and_tiles(State) , State };

request( <<"move_intent">>, <<"right">>, State = #client_state{ x = X } ) ->
  State1 = State#client_state{ x = X + 1 },
    case tile:move_object(  {State#client_state.x, State#client_state.y},
                            {State1#client_state.x, State1#client_state.y},
                            State#client_state.player_object ) of
    ok          -> { world_pos_and_tiles(State1), State1 };
    {error, _}  -> { world_pos_and_tiles(State), State }
  end;

request( <<"move_intent">>, <<"left">>, State = #client_state{ x = X } ) ->
  State1 = State#client_state{ x = X - 1 },
    case tile:move_object(  {State#client_state.x, State#client_state.y},
                            {State1#client_state.x, State1#client_state.y},
                            State#client_state.player_object ) of
    ok          -> { world_pos_and_tiles(State1), State1 };
    {error, _}  -> { world_pos_and_tiles(State), State }
  end;

request( <<"move_intent">>, <<"down">>, State = #client_state{ y = Y} ) ->
  State1 = State#client_state{ y = Y + 1 },
    case tile:move_object(  {State#client_state.x, State#client_state.y},
                            {State1#client_state.x, State1#client_state.y},
                            State#client_state.player_object ) of
    ok          -> { world_pos_and_tiles(State1), State1 };
    {error, _}  -> { world_pos_and_tiles(State), State }
  end;

request( <<"move_intent">>, <<"up">>, State = #client_state{ y = Y} ) ->
  State1 = State#client_state{ y = Y - 1 },
  case tile:move_object(  {State#client_state.x, State#client_state.y},
                          {State1#client_state.x, State1#client_state.y},
                          State#client_state.player_object ) of
    ok          -> { world_pos_and_tiles(State1), State1 };
    {error, _}  -> { world_pos_and_tiles(State), State }
  end;
  

request( Unknown, Data, State) ->
  {  {[{unknown_request, {[{ Unknown, Data }]} }]}, State }.

remove_player_from_world( State ) ->
  tile:remove_object( {State#client_state.x, State#client_state.y}, #thing{ type = o_player, state=#player_data{ username = State#client_state.username} } ).

view(X,Y, ViewSize) when is_integer(ViewSize) andalso ViewSize rem 2 /= 0 ->
  Delta = (ViewSize - 1) div 2,
  StartX = X - Delta, StartY = Y - Delta,
  EndX = X + Delta, EndY = Y + Delta,

  [ {
      <<(integer_to_binary(TileX))/binary,",",(integer_to_binary(TileY))/binary >>,
      add_keys_to_sprites(tile:sprites({TileX,TileY}) )
    } 
    || TileX <- lists:seq(StartX, EndX), TileY <- lists:seq(StartY,EndY) ].

add_keys_to_sprites( Sprites ) ->
  [ case SpriteData of
      {Bank, State} -> objects([{ bank , Bank }, { state, State }]);
      {Bank, State, Start} -> objects([{ bank , Bank }, { state, State }, {start, Start}])
    end || SpriteData <- Sprites ].

objects( Objects ) when is_list(Objects) ->
  { Objects }.

object( Key, Value ) ->
  {[{ Key, Value }]}.

login( Username, State = #client_state{ x = X, y = Y } ) ->
  PlayerObject = #thing{ type = o_player, state = #player_data{ username = Username } }, 
  tile:add_object( {X,Y}, PlayerObject ),
  State#client_state{ username = Username, player_object =  PlayerObject}.

world_pos_and_tiles( State ) ->
  WorldData = objects(view(State#client_state.x,State#client_state.y,15)),
  objects([
    {tile_data, WorldData},
    {world_pos, objects([
      {x, State#client_state.x},
      {y, State#client_state.y}
    ])}
    ]).

