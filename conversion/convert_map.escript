#!/usr/bin/env escript
%%! -smp enable -sname convertmap

main(["lex", FileName ]) ->
  true = code:add_pathz(filename:dirname(escript:script_name())),
  Tokens = lex(FileName),
  io:format("~p", [Tokens]);

main([ "parse", FileName]) ->
  true = code:add_pathz(filename:dirname(escript:script_name())),
  ParseTree = parse(FileName),
  io:format("~p", [ParseTree]).

parse(FileName) ->
  Tokens = lex(FileName),
  {ok, ParseTree} = map_parser:parse(Tokens),
  io:format("~p", [ParseTree]).

lex(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  Contents = binary_to_list(Bin),
  {ok, Tokens, _EndLine} = map_lexer:string(Contents),
  Tokens.