#!/usr/local/bin/escript
%%! -smp enable -sname convertmap
-define( BEGIN_TOKEN, 02 ).
-define( END_TOKEN, 03 ).
-mode(compile).

main(["lex", FileName ]) ->
  true = code:add_pathz(filename:dirname(escript:script_name())),
  io:format("%%%% Lexing file: ~p~n", [FileName]),
  Tokens = lex(FileName),
  io:format("~p", [Tokens]).

lex(FileName) ->
  {ok, Bin} = file:read_file(FileName),
  Contents = binary_to_list(Bin),
  StringsMarked = mark_strings_and_remove_comments( Contents, [] ),
  file:write_file( "/tmp/prelexed", StringsMarked ),
  {ok, Tokens, _EndLine} = dm_lexer:string(StringsMarked),
  Tokens.

mark_strings_and_remove_comments( [$" = EndToken | Rest] , Acc) ->
  in_string( EndToken, Rest, [?BEGIN_TOKEN|Acc] );

mark_strings_and_remove_comments( [$' = EndToken  | Rest] , Acc) ->
  in_string( EndToken, Rest, [?BEGIN_TOKEN|Acc] );

mark_strings_and_remove_comments( [ ${, $" | Rest] , Acc) ->
  hard_string( Rest, [?BEGIN_TOKEN|Acc]  );

mark_strings_and_remove_comments( [ $/, $/ | Rest], Acc ) ->
  in_cpp_comment( Rest, Acc );

mark_strings_and_remove_comments( [ $/, $* | Rest], Acc ) ->
  in_c_comment( Rest, Acc );

mark_strings_and_remove_comments( [H|T], Acc ) ->
  mark_strings_and_remove_comments( T, [H|Acc] );

mark_strings_and_remove_comments( [], Acc ) ->
  lists:reverse( Acc ).

in_c_comment( [ $*, $/ | T], Acc ) ->
  mark_strings_and_remove_comments( T, Acc );

in_c_comment( [_|T], Acc ) ->
  in_c_comment( T, Acc ).

in_cpp_comment( [ $/, $* | Rest], Acc ) ->
  in_c_comment( Rest, Acc );

in_cpp_comment( [ $\n | T], Acc ) ->
  mark_strings_and_remove_comments( T, [ $\n | Acc ]);

in_cpp_comment( [_|T], Acc ) ->
  in_cpp_comment( T, Acc );

in_cpp_comment ( [], Acc ) -> % EOF
  lists:reverse( Acc ).

in_string( EndToken, [ $\\, Escaped | Rest], Acc ) ->
  in_string( EndToken, Rest, [ Escaped, $\\ | Acc ] );

in_string( EndToken, [EndToken|Rest], Acc ) ->
  mark_strings_and_remove_comments( Rest, [?END_TOKEN|Acc] );

in_string( EndToken, [ $[ | Rest ], Acc ) ->
  {Rest1, Acc1} = subexpression( 1, Rest, [ $[ | Acc ] ),
  in_string( EndToken, Rest1, Acc1 );

in_string( EndToken, [H|T], Acc ) ->
  in_string( EndToken, T, [H|Acc] ).


subexpression( 1, [ $] | Rest ], Acc ) ->
  {Rest, [ $] | Acc ]};
subexpression( Level, [ $] | Rest ], Acc ) ->
  subexpression( Level - 1, Rest, [ $] | Acc ]);
subexpression( Level, [ $[ | Rest ], Acc ) ->
  subexpression( Level + 1, Rest, [ $[ | Acc ] );

subexpression( Level, [ $\\, $] | Rest ], Acc ) ->
  subexpression( Level, Rest, [ $], $\\ | Acc] );
subexpression( Level, [ $\\, $[ | Rest ], Acc ) ->
  subexpression( Level, Rest, [ $[, $\\ | Acc] );


subexpression( Level, [H|T], Acc ) ->
  subexpression( Level, T, [H|Acc] ).


hard_string( [ $", $} | Rest], Acc ) ->
  mark_strings_and_remove_comments( Rest, [?END_TOKEN|Acc] );

hard_string( [H|T], Acc ) ->
  hard_string( T, [H|Acc] ).