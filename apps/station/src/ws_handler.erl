-module(ws_handler).
-behaviour(cowboy_websocket_handler).
-include("sample_world_data.hrl").
-include("records.hrl").
 
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
 
init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.
 
websocket_init(_TransportName, Req, _Opts) ->
  timer:send_after( 0, need_login ),
  {ok, Req, #client_state{} }.
 
websocket_handle({text, Msg}, Req, State = #client_state{ username = undefined}) ->
  {Reply, State1} = case jiffy:decode(Msg) of 
    {[{ <<"username">>, User }]}  -> {?SAMPLE_WORLD_DATA, State#client_state{ username = User }};
    _                             -> {create_message( need_login ), State}
  end,

  {reply, {text, Reply}, Req, State1};
 
websocket_handle({text, _}, Req, State) ->
  Msg = ?SAMPLE_WORLD_DATA,
  {reply, {text, Msg}, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.
 
websocket_info(need_login, Req, State) ->
  {reply, {text, create_message( need_login )}, Req, State}.
 
websocket_terminate(_Reason, _Req, _State) ->
  ok.

create_message( need_login ) ->
  jiffy:encode( {[{ <<"need_login">>, <<"Please pass your username.">>}]} ).
