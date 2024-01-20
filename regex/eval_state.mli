type ('a, 's) t

val return : 'a -> ('a, 's) t
val bind   : ('a ,'s) t -> ('a -> ('b, 's) t) -> ('b, 's) t

val get    : ('s, 's) t
val set    : 's -> (unit, 's) t

val run    : 's -> ('a, 's) t -> 'a
