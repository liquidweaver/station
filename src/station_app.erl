-module(station_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [{'_', ws_handler, []}]}
    ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    {ok, ListenPort} = application:get_env( listen_port ),
    {ok, _Pid} = cowboy:start_http(http_listener, 100,
        [{port, ListenPort}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    station_sup:start_link().

stop(_State) ->
    ok.
