open Core

(* type eval_state =
  { pattern : 'a 
  ; text    : string
  ; pos     : int
  }
 *)
type 'a t = 'a Sequence.t

let return x = Sequence.of_list [x]

let rec bind m f =
  match Sequence.next m with
  | None         -> Sequence.empty 
  | Some (h, tl) -> Sequence.append (f h) (bind tl f)
  
let fail    = Sequence.empty

let flip    = Sequence.of_list [true; false]
let flipn n = Sequence.range 0 n

let run m   = m
