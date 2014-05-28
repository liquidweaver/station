-module(station_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, MapFile), {I, {I, start_link, [MapFile]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  PrivPath = case code:priv_dir( station ) of
      {error, bad_name} -> "priv";
      Path -> Path
  end,
  {ok, { {one_for_one, 5, 10}, [?CHILD(game_sup, supervisor, PrivPath ++ "/test.map")]} }.

