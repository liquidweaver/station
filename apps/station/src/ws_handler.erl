-module(ws_handler).
-behaviour(cowboy_websocket_handler).
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
 
websocket_handle({text, Msg}, Req, State) ->
  {ReplyType, ReplyData, State1} = try jiffy:decode(Msg) of
    {[{ MessageType, Data }]} -> client_handler:request( MessageType, Data, State )
  catch
    { error, Code } -> { error, Code, State }
  end,
  Reply = jiffy:encode( {[{ ReplyType, ReplyData }]}),
  {reply, {text, Reply}, Req, State1};
 
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.
 
websocket_info(need_login, Req, State) ->
  {reply, {text, jiffy:encode( {[{ need_login, <<"Please pass your username.">> }]} )}, Req, State}.
 
websocket_terminate(_Reason, _Req, _State) ->
  ok.

