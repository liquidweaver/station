-module(b_object).

-callback sprite( ObjectState :: map() ) -> Representation :: map().
-callback new( Args :: map() ) -> ObjectState :: map().