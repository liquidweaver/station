-module(o_door).
-behavior(b_object).
-export([ new/2, sprite/2, moved/2, blocks/2]).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

new( Coords, Args ) ->
  {ok, Pid}= start_link( Coords, Args ),
  #{ type => ?MODULE, pid => Pid }.

sprite( From, #{ pid := Pid } ) ->
  gen_server:call( Pid, {sprite, From} ).

moved({_From, To}, ObjectState ) ->
  ObjectState#{ coords => To }.

blocks(Other, ObjectState = #{ pid := Pid }) ->
  Block = gen_server:call( Pid, {blocks, Other} ),
  {Block, ObjectState}.

start_link( Coords, Args ) ->
  gen_server:start_link(?MODULE, {Coords, Args}, []).

init( { Coords, #{ status := Status } } ) ->
  {ok, #{ status => Status, coords => Coords, sprite_subscribers => sets:new() }}.

handle_call( {sprite, From}, _From, State = #{ status := Status, sprite_subscribers := Subscribers } ) ->
  SpriteMap = maps:merge( #{ type => ?MODULE }, status_to_bank_and_state( Status ) ),
  {reply, SpriteMap, State#{ sprite_subscribers => sets:add_element( From, Subscribers ) } };

handle_call( {blocks, _Other}, _From, State = #{ status := open } ) ->
  {reply, false, State };

handle_call( {blocks, _Other}, _From, State = #{ status := opening } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closing } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closed } ) -> % bumped
  timer:send_after( 3000, open ),
  {reply, true, notify_sprite_subscribers( State#{ status => opening } ) }.


handle_info(open, State) ->
  timer:send_after( 7000, closing ),
  {noreply, notify_sprite_subscribers( State#{ status => open } )};

handle_info(closing, State) ->
  timer:send_after( 3000, closed ),
  {noreply, notify_sprite_subscribers( State#{ status => closing } )};

handle_info(closed, State) ->
  {noreply, notify_sprite_subscribers( State#{ status => closed } )}.

handle_cast( _Args, State ) ->
  {noreply, State}.

notify_sprite_subscribers( State = #{ sprite_subscribers := Subscribers, coords := Coords, status := Status } ) ->
  case sets:size(Subscribers) of
    0 -> State;
    _ ->
      SpriteMap = maps:merge( #{ type => ?MODULE }, status_to_bank_and_state( Status ) ),
      sets:fold(  fun( Subscriber, _ ) ->
                        Subscriber ! {sprite, Coords, SpriteMap }
                      end, ignored, Subscribers ),
      State
  end.


status_to_bank_and_state( closed ) -> #{ bank => 'Door1', state => door1 };
status_to_bank_and_state( opening ) -> #{ bank => 'Door1', state => doorc0 };
status_to_bank_and_state( open ) -> #{ bank => 'Door1', state => door0 };
status_to_bank_and_state( closing ) -> #{bank => 'Door1', state => doorc1 }.