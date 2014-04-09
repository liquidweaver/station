-module(s_client).
-behavior(gen_server).

-include("records.hrl").

-export([start_link/0,init/1,terminate/2,handle_info/2]).
-export([handle_call/3]).
-export([view_world/0]).

-ifdef(TEST).
-include("private_tests/s_client_tests.hrl").
-endif.


start_link() ->				 %callback module, init parms, debug parms
	gen_server:start_link(?MODULE, [], []).

init([]) ->
	{ok, []}. % no state

handle_info(Msg, State) ->
	io:format("Unexpected message: ~p~n", [Msg]),
	{noreply, State}.

view_world() -> ok.

handle_call( view, _From, State = #client_state{ tiles = Tiles }) ->
	TilesString = tiles_to_string( Tiles ),
	{reply, TilesString, State}.

terminate( normal, _State) ->
	io:format("s_client terminated normally."),
	ok.


tiles_to_string( [] ) -> "";
tiles_to_string( Tiles ) ->
	{MinX, MinY, MaxX, MaxY} = min_max_coords( Tiles ),
	tiles_to_string( [], [], Tiles, MinX, MinY, MaxX, MaxY, MinX, MinY ).

tiles_to_string( Lines, Line, _Tiles, _MinX, _MinY, MaxX, MaxY, X, Y ) when X > MaxX andalso Y =:= MaxY ->
	LastLine = lists:reverse( Line ),
	lists:flatten( lists:reverse([LastLine|Lines]) );

tiles_to_string( Lines, Line, Tiles, MinX, MinY, MaxX, MaxY, X, Y ) when X > MaxX ->
	NewLine = lists:reverse( [ "\n" | Line ] ),
	tiles_to_string( [NewLine | Lines], [], Tiles, MinX, MinY, MaxX, MaxY, MinX, Y + 1);

tiles_to_string( Lines, Line, Tiles, MinX, MinY, MaxX, MaxY, X, Y ) ->
	{ Match, Rest } = lists:partition( fun( #tile{ x = Tx, y = Ty} ) ->
					X =:= Tx andalso Y =:= Ty
			end, Tiles),
	OutChar = case Match of
		[] -> "_";
		[Tile|_] -> Tile#tile.contents
	end,
  tiles_to_string( Lines, [OutChar | Line], Rest, MinX, MinY, MaxX, MaxY, X+1, Y).


min_max_coords( [ #tile{ x = X, y = Y} | Rest ] ) ->
	min_max_coords( Rest, X, Y, X, Y ).

min_max_coords( [ #tile{ x = X, y = Y} | Rest ], MinX, MinY, MaxX, MaxY ) ->
	{NewMinX, NewMaxX} = if
													X > MaxX -> {MinX, X};
													X < MinX -> {X, MaxX};
													true -> {MinX, MaxX}
											end,
	{NewMinY, NewMaxY} = if
													Y > MaxY -> {MinY, Y};
													Y < MinY -> {Y, MaxY};
													true -> {MinY, MaxY}
											end,
	min_max_coords( Rest, NewMinX, NewMinY, NewMaxX, NewMaxY );

min_max_coords( [],  MinX, MinY, MaxX, MaxY ) ->
	{MinX, MinY, MaxX, MaxY}.

create_tile( X, Y, Contents ) ->
	#tile{ x = X, y = Y, contents = Contents }.