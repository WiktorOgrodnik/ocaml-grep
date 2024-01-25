open Core

type 'a t = 'a Sequence.t
[@@deriving sexp_of]

val return : 'a -> 'a t
val bind   : 'a t -> ('a -> 'b t) -> 'b t

val fail   : 'a t
val flip   : bool t
val flipn  : int -> int t

val run : 'a t -> 'a Sequence.t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
