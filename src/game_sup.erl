-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(TILE_SUP_CHILD(I, Type, TileData), {I, {I, start_link, [TileData]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(MapFile) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, MapFile ).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(MapFile) ->
    {ok, [TileData]} = file:consult(MapFile),
    error_logger:info_report( [
      { mapfile, MapFile },
      { tiles, length(TileData) },
      { listen_port, element( 2, application:get_env(listen_port) )}
    ] ),
    {ok, { {one_for_all, 5, 10}, [?TILE_SUP_CHILD(tiles_sup, supervisor, TileData)]} }.