-module(o_floor).

-export([sprite/0]).

sprite() ->
  #{ type => ?MODULE, bank => floors, state => floor }.
