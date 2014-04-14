-module(tile).
-export([start_link/1]).
-include("records.hrl").

-export([sprites/1, add_object/2, move_object/3, accept_object/2]).

-export([coords_to_pid/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

sprites({X,Y}) ->
  case coords_to_pid({X,Y}) of
    undefined -> [{ space, 0 }];
    Pid       -> gen_server:call( Pid, sprites )
  end.

add_object( {X,Y}, Object ) when is_record( Object, thing ) ->
  case coords_to_pid({X,Y}) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:cast( Pid, {add_object, Object} )
  end.


move_object( From = {FX,FY}, To = {TX,TY}, Object = #thing{} ) ->
  case coords_to_pid(From) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {move_object, Object, To} )
  end.

accept_object( Coords = {X,Y}, Object ) when is_record( Object, thing) ->
  case coords_to_pid(Coords) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {accept_object, Object} )
  end.

start_link(Args = {X,Y}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []);

start_link(Args = {X,Y, _Contents}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []).

%% @private
init({X,Y, Contents} ) ->
  {ok, #tile_state{ x = X, y = Y, contents = Contents }};

init({X,Y}) ->
  {ok, #tile_state{ x = X, y =Y }}.

handle_call(get_contents, _From, State=#tile_state{ contents = Contents }) ->
  {reply, Contents, State };

handle_call(sprites, _From, State=#tile_state{ contents = Contents }) ->
  Sprites =  [Type:sprite() || #thing{ type = Type } <- Contents ],
  {reply, Sprites, State };

handle_call( {move_object, Object, To = {TX,TY} }, _From, State = #tile_state{ contents = Contents } ) ->
  % XXX check of object in contents
  Result = case tile:accept_object( To, Object ) of
    ok              -> {reply, ok, State1 = State#tile_state{ contents = Contents -- [Object] } };
    {error, Reason} -> {reply, {error, Reason}, State }
  end;


handle_call( {accept_object, Object}, _From, State = #tile_state{ contents = Contents } ) ->
  {reply, ok, State#tile_state{ contents = Contents ++ [Object] } }; %%% XXX Stubbed

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({add_object, Object}, State=#tile_state{ contents = Contents } ) ->
  NewState = State#tile_state{ contents = Contents ++ [Object] },
  {noreply, NewState};

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

coords_to_atom(Coords) ->
  binary_to_atom(term_to_binary(Coords), latin1).

coords_to_pid(Coords) ->
  whereis( coords_to_atom(Coords) ).