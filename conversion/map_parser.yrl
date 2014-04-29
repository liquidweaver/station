Nonterminals
declaration tiledefinitions tiledefinition objects object properties property elements element quadrants quadrant list map key_values key_value.

Terminals '(' ')' ',' ';' '=' '{' '}'
string map_quadrant path integer float property_name null collection.

Rootsymbol declaration.

declaration -> tiledefinitions quadrants          : { tile_definitions, '$1', quadrants, '$2' }.

tiledefinitions -> tiledefinition                 : '$1'.
tiledefinitions -> tiledefinition tiledefinitions : ['$1'] ++ '$2'.

tiledefinition -> string '=' '(' objects ')'      : {tile_def, unwrap('$1'), '$4'}.

objects -> object                                 : ['$1'].
objects -> object ',' objects                     : ['$1'] ++ '$3'.

object -> path                                    : {object, unwrap('$1')}.
object -> path '{' properties '}'                 : {object, unwrap('$1'), properties, maps:from_list('$3') }.

properties -> property                            : ['$1'].
properties -> property ';' properties             : ['$1'] ++ '$3'.

property -> property_name element                 : {unwrap('$1'), '$2'}.
property -> property_name null                    : {unwrap('$1'), undefined }.
property -> property_name list                    : {unwrap('$1'), '$2'}.
property -> property_name map                     : {unwrap('$1'), maps:from_list('$2')}.

list -> collection '(' elements ')'               : '$3'.
list -> collection '(' objects ')'                : '$3'.
map  -> collection '(' key_values ')'             : '$3'.

key_values -> key_value                           : ['$1'].
key_values -> key_value ',' key_values            : ['$1'] ++ '$3'.

key_value -> string '=' string                    : {unwrap('$1'), unwrap('$3')}.

elements -> element                               : ['$1'].
elements -> element ',' elements                  : ['$1'] ++ '$3'.

element -> string                                 : unwrap('$1').
element -> integer                                : unwrap('$1').
element -> float                                  : unwrap('$1').

quadrants -> quadrant                             : '$1'.
quadrants -> quadrant quadrants                   : ['$1'] ++ '$2'.

quadrant -> map_quadrant '=' '{' string '}'       : { quadrant, unwrap('$1'), unwrap('$4') }.

Erlang code.
unwrap({_,_,V}) -> V.
