-module(client_handler).

-export([request/3]).

-export([remove_player_from_world/1]).

request( <<"username">>, Username, State) when is_binary(Username) andalso byte_size(Username) > 0 ->
  LoggedInState = login_player( Username, State ),
  WorldPosAndTiles = world_pos_and_tiles(LoggedInState),
  {WorldPosAndTiles, LoggedInState };

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
    ok          -> { world_pos_and_tiles(NewState), NewState };
    {error, _}  -> { world_pos_and_tiles(OldState), OldState }
    end.

view(X,Y, ViewSize) when is_integer(ViewSize) andalso ViewSize rem 2 /= 0 ->
  Delta = (ViewSize - 1) div 2,
  StartX = X - Delta, StartY = Y - Delta,
  EndX = X + Delta, EndY = Y + Delta,

  Sprites = [ {
      <<(integer_to_binary(TileX))/binary,",",(integer_to_binary(TileY))/binary >>,
      tile:sprites({TileX,TileY})
    }
    || TileX <- lists:seq(StartX, EndX), TileY <- lists:seq(StartY,EndY) ],
  maps:from_list( Sprites ).

create_player_object( Username ) ->
  #{ type => o_player, username => Username  }.

login_player( Username, State ) ->
  PlayerObject = create_player_object(Username),
  tile:add_object( {7,7}, PlayerObject ),
  State#{ username => Username, player_object => PlayerObject, x => 7, y => 7}.

world_pos_and_tiles( #{ x := X, y := Y} ) ->
  WorldData = view(X,Y,15),
  #{ world_pos => #{ x => X, y => Y}, tile_data => WorldData }.