-module(map_codec).

-export([ encode/1, decode/1 ]).

encode( Map ) when is_map( Map ) ->
  Objectified = map_to_object( Map ),
  jiffy:encode( Objectified ).

decode( JSON ) when is_binary( JSON ) ->
  object_to_map( jiffy:decode(JSON) ).

map_to_object( Map ) ->
  FoldFun = fun(K,V, Acc) ->
              [ {K, term_to_object(V) } | Acc ]
  end,
  { maps:fold( FoldFun, [], Map ) }.

term_to_object( Map ) when is_map( Map ) ->
  map_to_object(Map);

term_to_object( List ) when is_list( List ) ->
  lists:map( fun term_to_object/1, List );

term_to_object( Tuple ) when is_tuple( Tuple ) ->
  term_to_object( tuple_to_list( Tuple ) );

term_to_object( Value ) ->
  Value.

object_to_map( {KVList} ) ->
  FoldFun = fun( {Key, Value}, Acc ) when is_tuple( Value )->
              maps:put( Key, object_to_map(Value), Acc);
            ( {Key,Value}, Acc ) ->
              maps:put( Key, Value, Acc)
  end,
  lists:foldl( FoldFun, maps:new(), KVList ).