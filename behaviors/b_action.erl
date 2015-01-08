-module(b_action).

-callback do_action( SourceObject :: map(), TargetObject :: map() ) -> { map(), map(), list() }.