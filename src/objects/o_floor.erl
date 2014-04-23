-module(o_floor).

-export([sprite/1, new/1]).

sprite(_) ->
  #{ type => ?MODULE, bank => floors, state => floor }.

new(_) ->
  #{ type => ?MODULE }.