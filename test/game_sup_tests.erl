-module(game_sup_tests).
-include_lib("eunit/include/eunit.hrl").

-define(TILE_SUP_CHILD(I, Type, TileData), {I, {I, start_link, [TileData]}, permanent, 5000, Type, [I]}).

init_starts_tile_sup_with_tile_data_from_map_file_supplied_test() ->
  meck:new( file, [unstick, passthrough] ),
  meck:expect( file, consult, 1, {ok, [[tiledata1]]} ),
  Expected = {ok, { {one_for_all, 5, 10}, [?TILE_SUP_CHILD(tiles_sup, supervisor, [tiledata1])]} },
  Actual = game_sup:init( filename1),
  io:format("~p", [Actual]),
  meck:called( file, consult, [filename1] ),
  meck:unload( file ),
  ?assertEqual( Expected, Actual ).