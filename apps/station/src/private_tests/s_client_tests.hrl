-include_lib("eunit/include/eunit.hrl").

create_tile_creates_a_tile_test() -> 
  ?assertEqual(
    #tile{ x = xcoord, y = ycoord, contents = somestuff},
    create_tile( xcoord, ycoord, somestuff ) ).

tiles_to_string_empty_returns_empty_string_test() ->
  ?assertEqual( "", tiles_to_string( [] ) ).

tile_to_string_returns_raster_representation_of_tiles_test() ->
  Tiles = [ create_tile( 0,0, "X" ), create_tile(1,1,"X"), create_tile(2,2,"B") ],
  Expected = "X__\n_X_\n__B",
  ?assertEqual( Expected, tiles_to_string( Tiles ) ).

min_max_coords_returns_min_and_max_coords_from_tiles_test() ->
  Tiles = [ create_tile( 0,0,stuff ), create_tile( 10,22,blah ), create_tile( 4,23,things ) ],

  ?assertEqual( {0, 0, 10, 23}, min_max_coords( Tiles ) ).