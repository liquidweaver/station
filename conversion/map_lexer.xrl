Definitions.
L       = [A-Za-z]
WS      = (\s|%.*)
NEWLINE = (\r\n|\n)
D       = [0-9]
PATH    = (/[A-Za-z0-9_]+)+

Rules.
\'(\\.|[^'])*\'     : S = strip( TokenChars),
                      {token, {string, TokenLine, S}}.
\"(\\.|[^"])*\"     : S = strip( TokenChars),
                      {token, {string, TokenLine, S}}.
\({D}+,{D}+,{D}+\)  : {token, {map_quadrant, TokenLine, TokenChars}}.
[\(\)\,\=\{\}\;]    : {token, {list_to_atom(TokenChars), TokenLine} }.
{PATH}              : {token, {path, TokenLine, TokenChars }}.
-?{D}+              : {token, {integer,TokenLine,list_to_integer(TokenChars)}}.
-?{D}+\.{D}+        : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{D}+e(\+|-){D}+     : Float = sci_to_float( TokenChars),
                      {token, {float, TokenLine, Float}}.
{NEWLINE}           : skip_token.
{WS}+               : skip_token.
{L}[A-Za-z0-9_]+\s= : S = strip_property(TokenChars, TokenLen ),
                      {token, {property_name, TokenLine, list_to_atom(S)}}.
null                : {token, {null, TokenLine}}.
list                : {token, {collection, TokenLine}}.

Erlang code.
sci_to_float( TokenChars ) ->
  {ok, [Significand,Exponent], _} = io_lib:fread( "~de~d", TokenChars ),
  Significand * math:pow(10, Exponent).

strip(TokenChars) ->
  NoReturns = lists:filter( fun(E) -> E =/= $\r andalso E =/= $\n end, TokenChars ),
  lists:sublist( NoReturns, 2, length(NoReturns) - 2).

strip_property(TokenChars, TokenLen) ->
  lists:sublist( TokenChars, 1, TokenLen - 2).