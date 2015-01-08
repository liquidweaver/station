-module(objects).
-export([equal/2, match_ref/2, create_ref/0, object_from_ref/2]).

-spec equal( map(), map() ) -> boolean().
equal( #{ ref := RefLeft }, #{ ref := RefRight} ) when RefLeft =:= RefRight ->
  true;
equal( _, _) ->
  false.

-spec match_ref( map(), integer() ) -> boolean().
match_ref( #{ ref := Reference }, Reference ) -> true;
match_ref( _, _) -> false.

-spec create_ref() -> integer().
create_ref() ->
  erlang:phash2( make_ref() ).

-spec object_from_ref( integer() | map(), [map()] ) -> map().
object_from_ref( ObjectRef, Contents ) when is_integer(ObjectRef) ->
  [Object] = [ O || O <- Contents, match_ref( O, ObjectRef ) ],
  Object;
object_from_ref( Object, _) when is_map( Object ) ->
  Object.