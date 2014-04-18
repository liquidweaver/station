-module(s_client_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

view_world_should_return_ok_test() ->
	?assertEqual( ok, s_client:view_world()).

%handle_call_view_should_return_string_of_world_test() ->
%	Tiles = [ #tile{ point=#point{x=50, y=50}, contents=x },
%						#tile{ point=#point{x=51, y=50}, contents=e }],
%	State = #s_client_state{ point = #point{ x = 50, y = 50 },
%						tiles = Tiles },
%	Expected = {reply,"xe", State},
%
%	Result = s_client:handle_call(view, from, State),
%
%	?assertEqual( Expected, Result ).
