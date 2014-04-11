-module(tile).
-export([start_link/1]).
-include("records.hrl").

-export([coords_to_pid/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link({X, Y}) ->
  gen_server:start_link({local, coords_to_atom(X,Y)}, ?MODULE, {X,Y}, []).

%% @private
init({X,Y}) ->
  {ok, #tile_state{ x = X, y =Y}}.

handle_call(get_contents, _From, State=#tile_state{ contents = Contents }) ->
  {reply, Contents, State };

handle_call(sprites, _From, State=#tile_state{ contents = Contents }) ->
  Sprites =  [Object:sprite() || Object <- Contents ],
  {reply, Sprites, State};

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

%% @private
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @private
handle_info(_Info, State) ->
  {noreply, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

coords_to_atom(X,Y) ->
  binary_to_atom(term_to_binary({X,Y}), latin1).

coords_to_pid(X,Y) ->
  whereis( coords_to_atom(X,Y) ).