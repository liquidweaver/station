-module(tile).
-export([start_link/1]).

-export([sprites/1, add_object/2, move_object/3, accept_object/3, remove_object/2, object_state/2, notify_update/1, remove_tile_subscription/2, source_action/5]).

-export([coords_to_pid/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

sprites(Coords = {X, Y}) ->
  case coords_to_pid(Coords) of
    undefined -> [#{ type => o_space, bank => space, state => erlang:phash( X * Y, 5 ) }];
    Pid       -> gen_server:call( Pid, sprites )
  end.

add_object( Coords, Object )  ->
  case coords_to_pid(Coords) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:cast( Pid, {add_object, Object} )
  end.

-spec move_object({ non_neg_integer(), non_neg_integer()}, { non_neg_integer(), non_neg_integer()}, Object :: map()) -> undefined | {ok, map()} | { error, atom() }.
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

object_state( Coords, ObjectRef ) ->
  case coords_to_pid(Coords) of
    undefined -> {error, no_tile};
    Pid       -> gen_server:call( Pid, {object_state, ObjectRef} )
  end.

-spec source_action( tuple(), tuple(), atom(), integer() | map(), integer() | map() ) -> ok.
source_action( FromTile, TargetTile, Action, Source, TargetRef ) ->
  { FromTilePid, TargetTilePid } = { coords_to_pid( FromTile ), coords_to_pid( TargetTile ) },
  gen_server:cast( FromTilePid, {source_action, Action, TargetTilePid, Source, TargetRef } ).

-spec notify_update( tuple() ) -> ok.
notify_update( Coords ) ->
  gen_server:cast( coords_to_pid(Coords), contents_changed ).

-spec remove_tile_subscription( tuple(), pid() ) -> ok.
remove_tile_subscription( Coords, Pid ) ->
  gen_server:cast( coords_to_pid(Coords), {remove_tile_subscription, Pid} ).

start_link(Args = {X,Y}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []);

start_link(Args = {X,Y, _Contents}) ->
  gen_server:start_link({local, coords_to_atom({X,Y})}, ?MODULE, Args, []).

%% @private
init({X,Y, Objects} ) ->
  Contents1 = [ Type:new( {X,Y}, State#{ ref => objects:create_ref(), type => Type } ) || {Type, State} <- Objects ],
  % Contents1 = [ Type:new( {X,Y}, State ) || {Type, State} <- Objects ],
  {ok, #{ x => X, y => Y, contents => Contents1, tile_subscribers => sets:new()  }};

init({X,Y}) ->
  {ok, #{ x => X, y => Y, contents => empty }}.

handle_call(get_contents, _From, State=#{ contents := Contents }) ->
  {reply, Contents, State };

handle_call(sprites, {FromPid, _}, State = #{ contents := Contents, tile_subscribers := Subscribers }) ->
  Sprites =  [ maps:put( ref, Ref, Type:sprite( Object ) ) || Object = #{ type := Type, ref := Ref } <- Contents ],

  {reply, Sprites, State#{ tile_subscribers => sets:add_element( FromPid, Subscribers ) } };

handle_call( {move_object, Object, To}, _From, State = #{ contents := Contents, x := X, y := Y } ) ->
  % XXX check of object in contents

  case tile:accept_object( {X,Y}, To, Object ) of
    {ok, MovedObject}  -> % TODO: demonitor actor if applicable
      NewContentsState = State#{ contents => Contents -- [Object] },
      send_sprites_to_subscribers( NewContentsState ),
      {reply, {ok, MovedObject}, NewContentsState};
    {error, Reason} -> {reply, {error, Reason}, State }
  end;

handle_call( {remove_object, Object}, _From, State = #{ contents := Contents } ) ->
  NewState = State#{ contents => Contents -- [Object] },
  send_sprites_to_subscribers( NewState ),
  {reply, ok, NewState };

handle_call( {accept_object, From, ProposedObject = #{ type := ObjectType }}, _From, State = #{ contents := Contents, x := X, y := Y } ) ->
  case lists:mapfoldl( fun( TestObject = #{ type := Type}, BlockedSoFar ) ->
                    {Block, NewObjectState } = Type:blocks( ProposedObject, TestObject ),
                    {NewObjectState, BlockedSoFar or Block}
                  end, false, Contents ) of % We update contents in case things are 'bumped'
    {Contents1, true}  -> {reply, {error, blocked}, State#{ contents := Contents1 } };
    {Contents1, false} -> % TODO: Monitor actor if applicable
      MovedObject = ObjectType:moved( {From, {X,Y}}, ProposedObject ),
      NewState = State#{ contents => Contents1 ++ [MovedObject] },
      send_sprites_to_subscribers( NewState ),
      {reply, {ok, MovedObject}, NewState }
  end;

% TODO: Process side effects
handle_call( {target_action, Action, SourceObject, TargetRef}, _From, State = #{contents := Contents } ) ->
  TargetObject = objects:object_from_ref( TargetRef, Contents ),
  ActionModule = binary_to_existing_atom( <<"a_", (atom_to_binary( Action, latin1 ))/binary >>, latin1 ),
  {NewSourceObject, NewTargetObject, _SideEffects} = ActionModule:do_action( SourceObject, TargetObject ),
  case TargetObject =:= NewTargetObject of
    true -> {reply, NewSourceObject, State };
    false ->
      Contents1 = update_contents( Contents, TargetObject, NewTargetObject ),
      NewState = State#{ contents := Contents1 },
      notify_objects_entity(NewTargetObject),
      send_sprites_to_subscribers(NewState), % TODO: Compare sprites first with Object:sprite/1
      {reply, NewSourceObject, NewState }
  end;

handle_call( {object_state, ObjectRef}, _From, State = #{ contents := Contents } ) ->
  Object = objects:object_from_ref( ObjectRef, Contents ),
  {reply, Object, State };

%% @private
handle_call(_Request, _From, State) ->
  {reply, {error, unknown_call}, State}.

handle_cast({add_object, Object}, State=#{ contents := Contents } ) ->
  NewState = State#{ contents => Contents ++ [Object] },
  send_sprites_to_subscribers( NewState ),
  {noreply, NewState};

handle_cast( contents_changed, State) ->
  send_sprites_to_subscribers( State ),
  {noreply, State};

handle_cast( {remove_tile_subscription, Pid }, State = #{ tile_subscribers := Subscribers } ) ->
  {noreply, State#{ tile_subscribers => sets:del_element( Pid, Subscribers) }};

handle_cast( { source_action, Action, TargetTile, SourceRefOrObject, TargetRef }, State = #{ contents := Contents } ) ->
  SourceObject = objects:object_from_ref( SourceRefOrObject, Contents ),
  NewSourceObject = gen_server:call( TargetTile, {target_action, Action, SourceObject, TargetRef } ),
  case SourceObject =:= NewSourceObject of
    true -> {noreply, State };
    false ->
      Contents1 = update_contents( Contents, SourceObject, NewSourceObject ),
      NewState = State#{ contents := Contents1 },
      notify_objects_entity(NewSourceObject),
      send_sprites_to_subscribers(NewState), % TODO: Compare sprites first with Object:sprite/1
      {noreply, NewState}
  end;

handle_cast(_Msg, State) -> {noreply, State}.

handle_info( {'DOWN', Ref, process, Pid, _Reason}, State = #{ contents := Contents } ) ->
  NewContents = lists:filter( fun
        (#{ pid := P, monitor_ref := R }) when P =:= Pid andalso R =:= Ref -> false;
        (_) -> true
      end, Contents ),
  NewState = State#{ contents => NewContents },
  send_sprites_to_subscribers( NewState ), % TODO: Inform subscribers of an "anomoly"
  {noreply, NewState };

handle_info(_Info, State) ->
  {noreply, State}.

terminate( _Reason, _State ) -> ignored.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

coords_to_atom(Coords) ->
  binary_to_atom(term_to_binary(Coords), latin1).

coords_to_pid(Coords) ->
  whereis( coords_to_atom(Coords) ).

send_sprites_to_subscribers( #{ contents := Contents, tile_subscribers := Subscribers, x := X, y := Y } ) ->
   case sets:size( Subscribers ) of
    0 -> noop;
    _ ->
      Sprites =  [Type:sprite( Object ) || Object = #{ type := Type } <- Contents ],
      sets:fold(fun(Subscriber, _) ->
                  %gen_server:cast( Subscriber, {tile_data, {X,Y}, Sprites} )
                  %%% XXX Need the player to be a proper entity
                  Subscriber ! {tile_data, {X,Y}, Sprites }
                end,ignore,Subscribers)
  end.

% TODO delete entity's actor it applicable
-spec update_contents( [map()], map(), map() | deleted ) -> [map()].
update_contents( Contents, OldObject, deleted ) ->
  lists:filter( fun( Object ) -> not objects:equal( OldObject, Object ) end, Contents );

update_contents( Contents, _OldObject, NewObject ) ->
  lists:map( fun (Object) ->
        case objects:equal(Object, NewObject) of
          true -> NewObject;
          false -> Object
        end
      end, Contents ).

notify_objects_entity( State = #{ pid := Pid } ) ->
  %gen_server:cast( Pid, {object_changed, State } ),
  %%% XXX Need the player to be a proper entity
  Pid ! {object_changed, State };
notify_objects_entity( _State ) -> noop.
