-module(b_object).

-callback new( Args :: map() ) -> ObjectState :: map().
-callback sprite( ObjectState :: map() ) -> Representation :: map().
-callback moving( ObjectState :: map() ) -> ObjectState :: map().     % MUST NOT HAVE SIDE EFFECTS!
-callback blocks( OtherObjectState :: map(), SelfObjectState :: map() ) -> { CanPass :: boolean(), SelfObjectState :: map() }. % Can return new state when 'bumped'