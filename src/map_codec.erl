-module(map_codec).

-export([ encode/1, decode/1 ]).

encode( Map ) when is_map( Map ) ->
  Objectified = map_to_object( Map ),
  jiffy:encode( Objectified ).

decode( JSON ) when is_binary( JSON ) ->
  object_to_map( jiffy:decode(JSON) ).

map_to_object( Map ) ->
  FoldFun = fun(K,V, Acc) when is_map(V) ->
              [{K, map_to_object(V)} | Acc ];
            (K,V, Acc) ->
              [ {K,V} | Acc ]
  end,
  { maps:fold( FoldFun, [], Map ) }.

object_to_map( {KVList} ) ->
  FoldFun = fun( {Key, Value}, Acc ) when is_tuple( Value )->
              maps:put( Key, object_to_map(Value), Acc);
            ( {Key,Value}, Acc ) ->
              maps:put( Key, Value, Acc)
  end,
  lists:foldl( FoldFun, maps:new(), KVList ).