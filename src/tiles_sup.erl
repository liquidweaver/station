-module(tiles_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-export([add_child/3]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Identifier, Module, Type, Objects), {Identifier, {Module, start_link, [Objects]}, permanent, 5000, Type, [Module]}).

start_link(GameMap) when is_list(GameMap) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, GameMap).

init(GameMap) ->
  Tiles = [ ?CHILD( {X,Y}, tile, worker,
                {X,Y, Objects }
            )
            || {{X,Y}, Objects} <- GameMap],
  {ok, { {one_for_one, 5, 10}, Tiles} }.

add_child(X,Y, Objects) ->
  supervisor:start_child( ?MODULE, ?CHILD( {X,Y}, tile, worker, {X,Y, Objects} ) ).