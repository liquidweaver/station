#!/usr/bin/env escript
%%! -smp enable -sname convertmap

main(["lex", FileName ]) ->
  true = code:add_pathz(filename:dirname(escript:script_name())),
  Tokens = lex(FileName),
  io:format("~p", [Tokens]).

lex(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  Contents = binary_to_list(Bin),
  {ok, Tokens, _EndLine} = dm_lexer:string(Contents),
  Tokens.