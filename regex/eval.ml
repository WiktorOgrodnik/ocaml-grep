open Core
open Ast

let (let* ) = Bt.bind

let select_from_alt ast =
  match ast with
  | ALTERNATIVE lst ->
      let  n  = List.length lst in
      let* a  = Bt.flipn n      in
      let elt = List.nth lst a  in
      begin match elt with 
      | Some t -> Bt.return t
      | None   -> Bt.fail
      end
  | _               -> Bt.fail 

let rec search_ast text pattern position =
  match pattern with
  | LITERAL     _ -> search_literal  text pattern position
  | SEQUENCE    _ -> search_sequence text pattern position
  | ALTERNATIVE _ -> search_alt      text pattern position
  | REPEATER    _ -> search_repeater text pattern position
  | _             -> Bt.fail (* Temp *)

and search_literal text pattern position =
  if position >= String.length text then Bt.fail
  else
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

and search_alt text pattern position =
  let* pattern = select_from_alt pattern in
  search_ast text pattern position

and search_repeater text pattern position =
  let some_or v o =
    match v with
    | Some v -> v
    | None   -> o
  in
  let rec search_aux position rep =
    match pattern with
    | REPEATER (t, r) ->
        let left_bound  = some_or r.l 0                            in
        let right_bound = some_or r.r Int.max_value                in
        let cond        = rep >= left_bound && rep <= right_bound  in
        let* flip       = Bt.flipn 2 in
        begin match flip with
        | 0 -> if cond then Bt.return (position - 1) else Bt.fail
        | _ ->
          let* position   = search_ast text t position in
          search_aux (position + 1) (rep + 1)
        end
    | _ -> Bt.fail
  in
  search_aux position 0

let search text pattern =
  let rec search_aux pos xs =
    if pos >= String.length text then xs
    else
      let res = search_ast text pattern pos in
      match Sequence.max_elt res ~compare:(Int.compare) with
      | None   -> search_aux (pos + 1) xs
      | Some t -> search_aux (t + 1) ((pos, t) :: xs)
  in
  search_aux 0 []
