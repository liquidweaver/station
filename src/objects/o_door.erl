-module(o_door).
-behavior(b_object).
-export([ new/2, sprite/1, moved/2, blocks/2]).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

new( Coords, Args ) ->
  {ok, Pid}= start_link( Coords, Args ),
  #{ type => ?MODULE, pid => Pid }.

sprite( #{ pid := Pid } ) ->
  gen_server:call( Pid, sprite ).

moved({_From, To}, ObjectState = #{ pid := Pid } ) ->
  gen_server:cast( Pid, {coords, To } ),
  ObjectState.

blocks(Other, ObjectState = #{ pid := Pid }) ->
  Block = gen_server:call( Pid, {blocks, Other} ),
  {Block, ObjectState}.

start_link( Coords, Args ) ->
  gen_server:start_link(?MODULE, {Coords, Args}, []).

init( { Coords, #{ status := Status } } ) ->
  {ok, #{ status => Status, coords => Coords}}.

handle_call( sprite, _From, State ) ->
  SpriteMap = state_to_sprite( State ),
  {reply, SpriteMap, State };

handle_call( {blocks, _Other}, _From, State = #{ status := open } ) ->
  {reply, false, State };

handle_call( {blocks, _Other}, _From, State = #{ status := opening } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closing } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closed, coords := Coords } ) -> % bumped
  tile:notify_update( Coords ),
  timer:send_after( game_time:tick_length(12), open ),
  {reply, true, State#{ status => opening, state_start => game_time:timestamp() } }.


handle_info(open, State = #{ coords := Coords }) ->
  tile:notify_update( Coords ),
  timer:send_after( 7000, closing ),
  {noreply, State#{ status => open } };

handle_info(closing, State = #{ coords := Coords }) ->
  tile:notify_update( Coords ),
  timer:send_after( game_time:tick_length(12), closed ),
  {noreply, State#{ status => closing, state_start => game_time:timestamp() } };

handle_info(closed, State = #{ coords := Coords }) ->
  tile:notify_update( Coords ),
  {noreply, State#{ status => closed } }.

handle_cast( {coords, Coords}, State ) ->
  {noreply, State#{ coords => Coords } };

handle_cast( _Args, State ) ->
  {noreply, State}.

state_to_sprite( #{ status := closed } ) ->
  #{ type => ?MODULE, bank => 'Door1', state => door1 };

state_to_sprite( #{ status := open } ) ->
  #{ type => ?MODULE, bank => 'Door1', state => door0 };

state_to_sprite( #{ status := opening, state_start := Start  } ) ->
  #{ type => ?MODULE, bank => 'Door1', state => doorc0, start => Start };

state_to_sprite( #{ status := closing, state_start := Start  }) ->
  #{ type => ?MODULE, bank => 'Door1', state => doorc1, start => Start }.
