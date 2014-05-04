Definitions.
WS                  = ([\s\t]+)
NEWLINE             = (\r\n|\n)
LINECONT            = \\\s*{NEWLINE}
MULTI_CHAR_OPERATOR = &&|\|\||>=|<=|==|!=|<>|<<|>>|<<=|>>=|\+=|-=|\*=|/=|\|=|\^=|&=|%=|\+\+|--|\*\*
SUPER               = \.\.\(\)
PREPROC             = #(define|if|elif|ifdef|ifndef|else|endif|include|error|warn|undef)
MACRO               = DM_VERSION|__FILE__|__LINE__|__MAIN__
D                   = [0-9]
L                   = [A-Za-z_]
BEGINSTRING         = \x02
ENDSTRING           = \x03
INFINITY            = 1\.#INF
NAN                 = 1\.#NAN

% 'off-side' grammar used by DM isn't context free. We give leex, a context-free lexer, context by way of
% a process dictionary storing a stack of indentation level, and emitting tokens when the indentation level changes.
% Empty lines are ignored w.r.t. to the indentation logic.
% Leex does not support emitting more than one token per match, so multiple detents are effected
% by way of pushing a special token back into the input stream, @detent.
% Because leex does not support look ahead, we are forced to push newlines back into the input steam for some rules.
% Unfortunately, this breaks leex's TokenLine reporting accuracy, so we keep track of the error offset in the process
% dictionary under the key newline_pushbacks. Additionally, we wrap returns of TokenLine in a helper function,
% real_line_number(), which corrects the line number.
% All in all, some ugly hacks here. Perhaps a rolling a custom lexer would have been better than twisting
% leex this much...
Rules.
@detent                                            : {token, {detent, real_line_number( TokenLine ) }}.
{WS}                                               : skip_token.
\{                                                 : {token, {indent, real_line_number( TokenLine ) }}.
\}                                                 : {token, {detent, real_line_number( TokenLine ) }}.
{PREPROC}                                          : {token, {preproc, real_line_number( TokenLine ), list_to_atom( TokenChars ) }}.
{LINECONT}                                         : skip_token.
{NEWLINE}{NEWLINE}                                 : inc_newline_pushback(), {skip_token, "\n"}.
{NEWLINE}{WS}*                                     : indention( real_line_number( TokenLine ), TokenChars ).
{SUPER}                                            : {token, {super,  real_line_number( TokenLine ) }}.
else\s+if                                          : {token, { elseif,  real_line_number( TokenLine ) }}.
for|if|else|new|return|in|as                       : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
{BEGINSTRING}[^\x03]*{ENDSTRING}                   : S = strip_string_tokens_and_remove_newlines( TokenChars),
                                                     {token, {string,  real_line_number( TokenLine ), S}}.
{INFINITY}                                         : {token, {infinity, real_line_number( TokenLine ) }}.
{NAN}                                              : {token, {not_a_number, real_line_number( TokenLine ) }}.
-?{D}+                                             : {token, {integer, real_line_number( TokenLine ),list_to_integer(TokenChars)}}.
-?{D}+\.{D}+                                       : {token, {float,  real_line_number( TokenLine ), list_to_float(TokenChars)}}.
{D}+e(\+|-){D}+                                    : Float = sci_to_float( TokenChars),
                                                     {token, {float,  real_line_number( TokenLine ), Float}}.
{MULTI_CHAR_OPERATOR}                              : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
[\(\)\,\[\]\;\'\"\~\?\:\.\/\-\+\<\>\&\%\!\*\^\|\=] : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
{L}+                                               : {token, {word,  real_line_number( TokenLine ), TokenChars }}.

Erlang code.

inc_newline_pushback() ->
  Pushbacks = case get(newline_pushbacks) of
    undefined -> 1;
    X         -> X + 1
  end,
  put( newline_pushbacks, Pushbacks ).

real_line_number( Number ) ->
  Pushbacks = case get(newline_pushbacks) of
    undefined -> 0;
    X         -> X
  end,
  Number - Pushbacks.

sci_to_float( TokenChars ) ->
  {ok, [Significand,Exponent], _} = io_lib:fread( "~de~d", TokenChars ),
  Significand * math:pow(10, Exponent).

indention( TokenLine, TokenChars ) ->
  Level = count_ws( TokenChars, 0 ),
  CurrentLevel = get_indent_level(),
  LineAfterNewline = TokenLine + 1, % Lets report indentation on the line that changes it (instead of newline)
  if
    CurrentLevel =:= Level ->
      skip_token;
    Level < CurrentLevel ->
      Detents = pop_indent_level( Level ),
      {token, {detent, LineAfterNewline }, Detents };
    Level > CurrentLevel ->
      push_indent_level( Level ),
      {token, {indent, LineAfterNewline } }
  end.

push_indent_level( Level ) ->
  NewIndentStack = case get(indent_stack ) of
    L when is_list(L) -> [Level|L];
    undefined -> [Level]
  end,
  put( indent_stack, NewIndentStack ).

pop_indent_level( Level ) ->
  [_|RestOfIndentStack] = get(indent_stack), %Toss top element, we are aleardy popping
  {NewIndentStack, Detents} = detents_from_indentstack( RestOfIndentStack, Level, [] ),
  put( indent_stack, NewIndentStack ),
  Detents.

detents_from_indentstack( IndentStack = [H|_], Level, Detents ) when H =< Level ->
  { IndentStack, Detents };

detents_from_indentstack( [], 0, Detents ) ->
  { [], Detents };

detents_from_indentstack( [H|T], Level, Detents ) when H > Level ->
  detents_from_indentstack( T, Level, "@detent" ++ Detents ).

get_indent_level() ->
  case get(indent_stack) of
    undefined -> -1;
    []        -> -1;
    [Current|_] -> Current
  end.

count_ws([H|T], Level) when H =:= $\s orelse H =:= $\t ->
  count_ws( T, Level + 1);

count_ws([_|T], Level ) -> count_ws( T, Level );
count_ws( []  , Level ) -> Level.

strip_string_tokens_and_remove_newlines(TokenChars) ->
  NoReturns = re:replace(TokenChars, "\\\r?\n", "", [global,{return,list}]),
  lists:sublist( NoReturns, 2, length(NoReturns) - 2).