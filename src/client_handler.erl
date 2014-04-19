-module(client_handler).

-export([request/3]).

-export([objects/1, object/2, view/3, remove_player_from_world/1]). %debugging

request( <<"username">>, Username, State) when
  is_binary(Username) andalso byte_size(Username) > 0 ->

  LoggedInState = login(Username, State#{ x => 7, y => 7}),
  {world_pos_and_tiles(LoggedInState), LoggedInState };

request( <<"username">>, _, State) ->
  { object(error, <<"Invalid username.">>), State };

request( _, _, State = #{ username := undefined }) ->
  { object(need_login, <<"Please pass your username.">>), State};

request( <<"send_tiles">>, _, State) ->
  {world_pos_and_tiles(State) , State };

request( <<"move_intent">>, <<"right">>, State = #{ x := X } ) ->
  State1 = State#{ x => X + 1 },
  move_player( State, State1 );

request( <<"move_intent">>, <<"left">>, State = #{ x := X } ) ->
  State1 = State#{ x => X - 1 },
  move_player( State, State1 );

request( <<"move_intent">>, <<"down">>, State = #{ y := Y} ) ->
  State1 = State#{ y => Y + 1 },
  move_player( State, State1 );

request( <<"move_intent">>, <<"up">>, State = #{ y := Y} ) ->
  State1 = State#{ y => Y - 1 },
  move_player( State, State1 );

request( Unknown, Data, State) ->
  {  {[{unknown_request, {[{ Unknown, Data }]} }]}, State }.

remove_player_from_world( #{ x := X, y := Y, player_object := PlayerObject } ) ->
  tile:remove_object( {X, Y}, PlayerObject ).

move_player( OldState = #{ x := OldX, y := OldY }, NewState = #{ x := NewX, y := NewY, player_object := PlayerObject } ) ->
    case tile:move_object(  {OldX, OldY}, {NewX, NewY}, PlayerObject ) of
    ok          -> { world_pos_and_tiles(NewState), NewState };
    {error, _}  -> { world_pos_and_tiles(OldState), OldState }
    end.

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
      {Id, Bank, State} -> objects([{ object_id, Id }, { bank , Bank }, { state, State }]);
      {Id, Bank, State, Start} -> objects([{ object_id, Id}, { bank , Bank }, { state, State }, {start, Start}])
    end || SpriteData <- Sprites ].

objects( Objects ) when is_list(Objects) ->
  { Objects }.

object( Key, Value ) ->
  {[{ Key, Value }]}.

login( Username, State = #{ x := X, y := Y } ) ->
  PlayerObject = #{ type => o_player, username => Username  },
  tile:add_object( {X,Y}, PlayerObject ),
  State#{ username => Username, player_object => PlayerObject}.

world_pos_and_tiles( #{ x := X, y := Y} ) ->
  WorldData = objects(view(X,Y,15)),
  objects([
    {tile_data, WorldData},
    {world_pos, objects([
      {x, X},
      {y, Y}
    ])}
    ]).

