-module(tile).
-export([start_link/1]).

-export([sprites/1, add_object/2, move_object/3, accept_object/3, remove_object/2]).

-export([coords_to_pid/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

sprites(Coords) ->
  case coords_to_pid(Coords) of
    undefined -> [#{ type => o_space, bank => space, state => 0 }];
    Pid       -> gen_server:call( Pid, sprites )
  end.

add_object( Coords, Object )  ->
  case coords_to_pid(Coords) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:cast( Pid, {add_object, Object} )
  end.

move_object( From, To, Object ) ->
  case coords_to_pid(From) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {move_object, Object, To} )
  end.

accept_object( From, To, Object ) ->
  case coords_to_pid(To) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {accept_object, From, Object} )
  end.

remove_object( Coords, Object ) ->
  % TODO Remove callback to notify actor-objects
  case coords_to_pid(Coords) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {remove_object, Object} )
  end.

start_link(Args = {X,Y}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []);

start_link(Args = {X,Y, _Contents}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []).

%% @private
init({X,Y, Contents} ) ->
  {ok, #{ x => X, y => Y, contents => Contents }};

init({X,Y}) ->
  {ok, #{ x => X, y => Y, contents => empty }}.

handle_call(get_contents, _From, State=#{ contents := Contents }) ->
  {reply, Contents, State };

handle_call(sprites, {FromPid, _}, State = #{ contents := Contents }) ->
  Sprites =  [Type:sprite( FromPid, Object ) || Object = #{ type := Type } <- Contents ],
  {reply, Sprites, State };

handle_call( {move_object, Object, To}, _From, State = #{ contents := Contents, x := X, y := Y } ) ->
  % XXX check of object in contents

  case tile:accept_object( {X,Y}, To, Object ) of
    {ok, MovedObject}  -> {reply, {ok, MovedObject}, State#{ contents => Contents -- [Object] } };
    {error, Reason} -> {reply, {error, Reason}, State }
  end;

handle_call( {remove_object, Object}, _From, State = #{ contents := Contents } ) ->
  {reply, ok, State#{ contents => Contents -- [Object] } };

handle_call( {accept_object, From, ProposedObject = #{ type := ObjectType }}, _From, State = #{ contents := Contents, x := X, y := Y } ) ->
  case lists:mapfoldl( fun( TestObject = #{ type := Type}, BlockedSoFar ) ->
                    {Block, NewObjectState } = Type:blocks( ProposedObject, TestObject ),
                    {NewObjectState, BlockedSoFar or Block}
                  end, false, Contents ) of % We update contents in case things are 'bumped'
    {Contents1, true}  -> {reply, {error, blocked}, State#{ contents := Contents1 } };
    {Contents1, false} ->
      MovedObject = ObjectType:moved( {From, {X,Y}}, ProposedObject ),
      {reply, {ok, MovedObject}, State#{ contents => Contents1 ++ [MovedObject] } }
  end;

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({add_object, Object}, State=#{ contents := Contents } ) ->
  NewState = State#{ contents => Contents ++ [Object] },
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