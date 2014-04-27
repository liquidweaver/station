-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  timer:send_after( 0, need_login ),
  {ok, Req, #{ username => undefined } }.

websocket_handle({text, Msg}, Req, State) ->
  {ReplyData, State1} = try jiffy:decode(Msg) of
    {[{ MessageType, Data }]} -> client_handler:request( MessageType, Data, State )
  catch
    { error, Code } -> { #{error => Code}, State }
  end,
  Reply = map_codec:encode(ReplyData),
  {reply, {text, Reply}, Req, State1};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({tile_data, {TileX,TileY}, Sprites }, Req, State ) ->
  CoordsBin = <<(integer_to_binary(TileX))/binary,",",(integer_to_binary(TileY))/binary >>,
  Reply = map_codec:encode( #{tile_data => maps:put( CoordsBin, Sprites, #{}) }),
  {reply, {text, Reply}, Req, State};

websocket_info(need_login, Req, State) ->
  {reply, {text, map_codec:encode(#{ need_login => <<"Please pass your username.">> } )}, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
  client_handler:remove_player_from_world(State).

