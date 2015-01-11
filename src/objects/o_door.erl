-module(o_door).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(b_object).
-export([ new/2, sprite/1, moved/2, blocks/2, actions/1]).

new( Coords, State ) ->
  {ok, Pid, MonRef } = start( Coords, State ),
  State#{ pid => Pid, monitor_ref => MonRef }.

sprite( #{ pid := Pid } ) ->
  gen_server:call( Pid, sprite ).

moved({_From, To}, ObjectState = #{ pid := Pid } ) ->
  gen_server:cast( Pid, {coords, To } ),
  ObjectState.

blocks(Other, ObjectState = #{ pid := Pid }) ->
  Block = gen_server:call( Pid, {blocks, Other} ),
  {Block, ObjectState}.

actions(_) -> [].

start( Coords, Args ) ->
  {ok, Pid} = gen_server:start(?MODULE, {Coords, Args}, []),
  {ok, Pid, monitor( process, Pid )}.

init( { Coords, #{ status := Status } } ) ->
  {ok, #{ status => Status, coords => Coords, state_start => game_time:timestamp() }}.

handle_call( sprite, _From, State ) ->
  SpriteMap = status_to_sprite( State ),
  {reply, SpriteMap, State };

handle_call( {blocks, _Other}, _From, State = #{ status := open } ) ->
  {reply, false, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closed } ) -> % bumped
  {reply, true, transition( opening, State ) };

handle_call( {blocks, _Other}, _From, State) ->
  {reply, true, State }.

handle_info(closed, State = #{ coords := Coords }) ->
  tile:notify_update( Coords ),
  {noreply, State#{ status => closed } };

handle_info( Transition, State ) ->
  NewState = transition( Transition, State ),
  {noreply, NewState }.

handle_cast( {coords, Coords}, State ) ->
  {noreply, State#{ coords => Coords } }.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate( _Reason, _State ) -> ignored.

transition( To, State = #{ coords := Coords } ) ->
  NextState = case To of
    opening -> open;
    open    -> closing;
    closing -> closed
  end,
  tile:notify_update( Coords ),
  timer:send_after( game_time:tick_length(12), NextState ),
  State#{ status => To, state_start => game_time:timestamp() }.

status_to_sprite( #{ status := Status, state_start := Start } ) ->
  #{ type => ?MODULE, bank => 'Door1', state => sprite_state(Status), start => Start }.

sprite_state( closed )  -> door1;
sprite_state( open )    -> door0;
sprite_state( opening ) -> doorc0;
sprite_state( closing ) -> doorc1.