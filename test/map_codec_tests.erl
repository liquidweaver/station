-module(map_codec_tests).
-include_lib("eunit/include/eunit.hrl").

encode_creates_binary_json_from_a_map_test() ->
  Result = map_codec:encode( #{ key => value } ),
  ?assertEqual( <<"{\"key\":\"value\"}">>, Result ).

encode_creates_binary_json_from_a_nested_map_test() ->
  Result = map_codec:encode( #{ key => #{ foo => baz } } ),
  ?assertEqual( <<"{\"key\":{\"foo\":\"baz\"}}">>, Result ).

passing_a_map_through_encode_and_decode_returns_original_map_test() ->
  TestMap = #{ <<"foo">> => <<"bar">>, <<"baz">> => <<"quux">> },
  ?assertEqual( TestMap, map_codec:decode( map_codec:encode( TestMap ) ) ).


passing_a_nested_map_through_encode_and_decode_returns_original_map_test() ->
  TestMap = #{ <<"foo">> => #{<<"baz">> => <<"quux">> } },
  ?assertEqual( TestMap, map_codec:decode( map_codec:encode( TestMap ) ) ).