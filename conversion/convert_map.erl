#!/usr/bin/env escript
%%! -smp enable -sname convertmap

main(FileName) ->
  true = code:add_pathz(filename:dirname(escript:script_name())),
  parse(FileName).

parse(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    Contents = binary_to_list(Bin),
    Symbols = map_lexer:string(Contents),
    io:format("~p", [Symbols]).