Definitions.
CCOMMENT            = (\/\*([^*]*\*+[^/*])*[^*]*\*+\/)
CPPCOMMENT          = (//.*)
WS                  = ([\s\t]+)
ABS_PATH            = (/[A-Za-z0-9_]+)+
NEWLINE             = (\r\n|\n)
MULTI_CHAR_OPERATOR = &&|\|\||>=|<=|==|!=|<>|<<|>>|<<=|>>=|\+=|-=|\*=|/=|\|=|\^=|&=|%=|\+\+|--|\*\*
SUPER               = \.\.\(\)
PREPROC             = #(define|if|elif|ifdef|ifndef|else|endif|include|error|warn)
MACRO               = DM_VERSION|__FILE__|__LINE__|__MAIN__
D                   = [0-9]
L                   = [A-Za-z_]

Rules.
{WS}                                               : skip_token.
{CCOMMENT}                                         : {token, {comment, TokenLine, strip_c_comment( TokenChars ) }}.
{CPPCOMMENT}                                       : {token, {comment, TokenLine, strip_cpp_comment( TokenChars ) }}.
{PREPROC}                                          : {token, {preproc, real_line_number( TokenLine ), list_to_atom(TokenChars) }}.
{NEWLINE}{WS}*{CCOMMENT}                           : {token, {comment, TokenLine, strip_c_comment( strip_nl_and_ws( TokenChars ) ) }}.
{NEWLINE}{WS}*{CPPCOMMENT}                         : {token, {comment, TokenLine, strip_cpp_comment( strip_nl_and_ws( TokenChars ) ) }}.
{NEWLINE}{NEWLINE}                                 : inc_newline_pushback(), {skip_token, "\n"}.
{NEWLINE}{WS}*                                     : Level = count_ws( TokenChars, 0 ),
                                                     {token, {indent, real_line_number( TokenLine ), Level }}.
{L}+{ABS_PATH}                                     : {token, {rel_path, real_line_number( TokenLine ), TokenChars }}.
{ABS_PATH}                                         : {token, {abs_path,  real_line_number( TokenLine ), TokenChars }}.
{SUPER}                                            : {token, {super,  real_line_number( TokenLine ) }}.
{DEFINE}                                           : {token, {define,  real_line_number( TokenLine ) }}.
\'(\\.|[^'])*\'                                    : S = strip( TokenChars),
                                                     {token, {hard_string,  real_line_number( TokenLine ), S}}.
\"(\\.|[^"])*\"                                    : S = strip( TokenChars),
                                                     {token, {string,  real_line_number( TokenLine ), S}}.
-?{D}+                                             : {token, {integer, real_line_number( TokenLine ),list_to_integer(TokenChars)}}.
-?{D}+\.{D}+                                       : {token, {float,  real_line_number( TokenLine ), list_to_float(TokenChars)}}.
{D}+e(\+|-){D}+                                    : Float = sci_to_float( TokenChars),
                                                     {token, {float,  real_line_number( TokenLine ), Float}}.
{MULTI_CHAR_OPERATOR}                              : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
[\(\)\,\[\]\;\'\"\:\.\/\~\?\-\+\<\>\&\%\!\*\^\|\=] : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
else\s+if                                          : {token, { elseif,  real_line_number( TokenLine ) }}.
for|if|else|new|return|in|as                       : {token, {list_to_atom(TokenChars),  real_line_number( TokenLine ) }}.
{L}+                                               : {token, {word,  real_line_number( TokenLine ), TokenChars }}.

Erlang code.
strip_nl_and_ws( String ) ->
  re:replace(String, "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

strip_c_comment( String ) ->
  strip_nl_and_ws(lists:sublist( String, 3, length(String) - 4 )).

strip_cpp_comment( String ) ->
  strip_nl_and_ws(lists:sublist( String, 3, length(String) - 2 )).

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

count_ws([H|T], Level) when H =:= $\s orelse H =:= $\t ->
  count_ws( T, Level + 1);

count_ws([_|T], Level ) -> count_ws( T, Level );
count_ws( []  , Level ) -> Level.

strip(TokenChars) ->
  NoReturns = lists:filter( fun(E) -> E =/= $\r andalso E =/= $\n end, TokenChars ),
  lists:sublist( NoReturns, 2, length(NoReturns) - 2).