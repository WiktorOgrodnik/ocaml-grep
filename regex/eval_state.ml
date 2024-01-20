type ('a, 's) t = 's -> 'a * 's

let return x s = (x, s)
let bind m f s =
  let (x, s) = m s in
  f x s

let get s = (s, s)
let set s _ = ((), s)

let run s m = fst (m s)
