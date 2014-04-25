-module(o_door).
-behavior(b_object).
-export([ new/1, sprite/1, moving/2, blocks/2]).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

new( Args ) ->
  {ok, Pid}= start_link( Args ),
  #{ type => ?MODULE, pid => Pid }.

sprite( #{ pid := Pid } ) ->
  gen_server:call( Pid, sprite ).

moving({_From, _To}, ObjectState ) ->
  ObjectState. % noop

blocks(Other, ObjectState = #{ pid := Pid }) ->
  Block = gen_server:call( Pid, {blocks, Other} ),
  {Block, ObjectState}.

start_link( Args ) ->
  gen_server:start_link(?MODULE, Args, []).

init( #{ status := Status } ) ->
  {ok, #{ status => Status }}.

handle_call( sprite, _From, State = #{ status := Status } ) ->
  SpriteMap = maps:merge( #{ type => ?MODULE }, status_to_bank_and_state( Status ) ),
  {reply, SpriteMap, State};

handle_call( {blocks, _Other}, _From, State = #{ status := open } ) ->
  {reply, false, State };

handle_call( {blocks, _Other}, _From, State = #{ status := opening } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closing } ) ->
  {reply, true, State };

handle_call( {blocks, _Other}, _From, State = #{ status := closed } ) -> % bumped
  timer:send_after( 5000, open ),
  {reply, true, State#{ status => opening } }.


handle_info(open, State) ->
  timer:send_after( 15000, closing ),
  {noreply, State#{ status => open }};

handle_info(closing, State) ->
  timer:send_after( 5000, closed ),
  {noreply, State#{ status => closing }};

handle_info(closed, State) ->
  {noreply, State#{ status => closed }}.

handle_cast( _Args, State ) ->
  {noreply, State}.


status_to_bank_and_state( closed ) -> #{ bank => 'Door1', state => door1 };
status_to_bank_and_state( opening ) -> #{ bank => 'Door1', state => doorc0 };
status_to_bank_and_state( open ) -> #{ bank => 'Door1', state => door0 };
status_to_bank_and_state( closing ) -> #{bank => 'Door1', state => doorc1 }.