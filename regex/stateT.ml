open Core

type ('a, 's) t = 's -> ('a * 's) Choice.t

let return x = fun s -> Choice.return (x, s)
let bind m f =
  let ( >>= ) = Choice.bind in fun s ->
  (m s) >>= (fun (a, s) -> f a s)

let get s   = Choice.return (s, s)
let set s _ = Choice.return ((), s) 

let run s m =
  let seq = Choice.run (m s) in
  match Sequence.next seq with
  | None -> None
  | Some (h, _) -> Some (fst h)
