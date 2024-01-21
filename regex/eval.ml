open Core
open Ast

let (let* ) = Bt.bind

let rec select a b =
  if   a >= b then Bt.fail
  else 
    let* c = Bt.flip in
    if c then Bt.return a
    else select (a + 1) b

(* let select_from_alt ast =
  match ast with
  | ALTERNATIVE lst ->
      let  n = List.length lst  in
      let* a = Bt.flipn n in
      let elt = List.nth lst a  in
      begin match elt with 
      | Some t -> Bt.return t
      | None   -> Bt.fail
      end
  | _               -> Bt.fail 
;;
 *)

let triples n =
  let* a = select 1 n in
  let* b = select a n in
  let* c = select b n in
  if a*a + b*b = c*c then Bt.return (a, b, c)
  else Bt.fail

let rec search_ast text pattern position =
  match pattern with
  | LITERAL     _ -> search_literal  text pattern position
  | SEQUENCE    _ -> search_sequence text pattern position
  (* | ALTERNATIVE _ -> search_alt      text pattern position *)
  | _             -> Bt.fail (* Temp *)

and search_literal text pattern position =
  let ch = String.get text position in
  let (=) = Char.equal              in
  match pattern with
  | LITERAL (Some c) when c = ch -> Bt.return position
  | LITERAL None                 -> Bt.return position
  | _                            -> Bt.fail

and search_sequence text pattern position =
  let rec search_aux pattern position =
    match Ast.seq_get_elt pattern with
    | Some (h, tl) ->
        let* position = search_ast text h position in
        search_aux tl (position + 1)
    | None         ->
        Bt.return (position - 1)
  in
  search_aux pattern position

(* and search_alt text pattern position =
  let rec search_aux pattern =
    let* pattern = select_from_alt pattern in
    search_ast text pattern position
  in
  search_aux pattern
 *)

let search text pattern =
  let rec search_aux pos xs =
    if pos >= String.length text then xs
    else
      let res = search_ast text pattern pos in
      match Sequence.max_elt res ~compare:(Int.compare) with
      | None   -> search_aux (pos + 1) xs
      | Some t -> search_aux (pos + 1) ((pos, t) :: xs)
  in
  search_aux 0 []
