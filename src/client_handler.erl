-module(client_handler).

-export([request/3]).

-export([remove_player_from_world/1]).

request( <<"username">>, Username, State) when is_binary(Username) andalso byte_size(Username) > 0 ->
  LoggedInState = login_player( Username, State ),
  {WorldPosAndTiles, KnownTilesState} = world_pos_and_tiles(LoggedInState),
  {WorldPosAndTiles#{ tick_length => game_time:tick_length(1) }, KnownTilesState };

request( <<"username">>, _, State) ->
  { #{ error => <<"Invalid username.">>}, State };

request( _, _, State = #{ username := undefined }) ->
  { #{ need_login => <<"Please pass your username.">>}, State};

request( <<"send_tiles">>, _, State) ->
  {world_pos_and_tiles(State) , State };

request( <<"move_intent">>, <<"right">>, State = #{ x := X } ) ->
  ProposedState = State#{ x => X + 1 },
  move_player( State, ProposedState );

request( <<"move_intent">>, <<"left">>, State = #{ x := X } ) ->
  ProposedState = State#{ x => X - 1 },
  move_player( State, ProposedState );

request( <<"move_intent">>, <<"down">>, State = #{ y := Y} ) ->
  ProposedState = State#{ y => Y + 1 },
  move_player( State, ProposedState );

request( <<"move_intent">>, <<"up">>, State = #{ y := Y} ) ->
  ProposedState = State#{ y => Y - 1 },
  move_player( State, ProposedState );

request( Unknown, Data, State) ->
  { #{ unknown_request => [Unknown, Data] }, State }.

remove_player_from_world( #{ x := X, y := Y, player_object := PlayerObject } ) ->
  tile:remove_object( {X, Y}, PlayerObject ).

move_player( OldState = #{ x := OldX, y := OldY }, NewState = #{ x := NewX, y := NewY, player_object := PlayerObject } ) ->
    case tile:move_object( {OldX, OldY}, {NewX, NewY}, PlayerObject ) of
    {ok, NewObject } -> world_pos_and_tiles(NewState#{ player_object => NewObject });
    {error, _}       -> world_pos_and_tiles(OldState)
    end.

view(X,Y, KnownTiles, ViewSize) when is_integer(ViewSize) andalso ViewSize rem 2 /= 0 ->
  Delta = (ViewSize - 1) div 2,
  StartX = X - Delta, StartY = Y - Delta,
  EndX = X + Delta, EndY = Y + Delta,
  PossibleTiles = [ {TileX,TileY} || TileX <- lists:seq(StartX, EndX), TileY <- lists:seq(StartY,EndY) ],
  TilesToRequest = PossibleTiles -- KnownTiles,
  Sprites = [ {
      <<(integer_to_binary(TileX))/binary,",",(integer_to_binary(TileY))/binary >>,
      tile:sprites({TileX,TileY})
    }
    || {TileX, TileY} <- TilesToRequest ],
  {maps:from_list(Sprites), PossibleTiles}.

create_player_object( Coords, Username ) ->
  o_player:new( Coords, #{name => Username} ).

login_player( Username, State ) ->
  PlayerObject = create_player_object({7,7}, Username),
  tile:add_object( {7,7}, PlayerObject ),
  State#{ username => Username, player_object => PlayerObject, x => 7, y => 7 }.

world_pos_and_tiles( State = #{ x := X, y := Y, known_tiles := KnownTiles } ) ->
  {WorldData, NewKnownTiles} = view(X,Y,KnownTiles,15),
  {#{ world_pos => #{ x => X, y => Y}, tile_data => WorldData }, State#{ known_tiles => NewKnownTiles }}.
