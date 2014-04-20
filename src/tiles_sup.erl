-module(tiles_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-export([add_child/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Identifier, Module, Type, Coords), {Identifier, {Module, start_link, [Coords]}, permanent, 5000, Type, [Module]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Tiles = [ case X =:= Y of
              true -> ?CHILD( {X,Y}, tile, worker, {X,Y,[#{type => o_floor}, #{type => o_cryosleeper}]} );
              false -> ?CHILD( {X,Y}, tile, worker, {X,Y,[#{type => o_floor}]} )
            end || X <- lists:seq(0,29), Y <- lists:seq(0,29) ],
  {ok, { {one_for_one, 5, 10}, Tiles} }.

add_child(X,Y) ->
  supervisor:start_child( ?MODULE, ?CHILD( {X,Y}, tile, worker, {X,Y} ) ).
