-module(b_object).

-callback new( Coords :: {pos_integer(), pos_integer()}, Args :: map() ) -> ObjectState :: map().
-callback sprite( From :: pid(), ObjectState :: map() ) -> Representation :: map().
-callback moved( {  From :: {pos_integer(), pos_integer()},
                    To :: {pos_integer(), pos_integer()}},
                    ObjectState :: map() )
          -> ObjectState :: map().
-callback blocks( OtherObjectState :: map(), SelfObjectState :: map() ) -> { CanPass :: boolean(), SelfObjectState :: map() }. % Can return new state when 'bumped'