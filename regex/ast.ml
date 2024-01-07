type node = (* -- Expression*)
| SEQUENCE of atom list
| ALTERNATIVE of atom list

and atom =
| REPEATED of int * group
| SINGLE   of group

and group =
| LITERAL of char option
| NODE of node