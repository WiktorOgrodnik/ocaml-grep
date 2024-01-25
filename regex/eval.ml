open Core
open Ast

let (let*  ) = Choice.bind

let select_from_alt ast =
  match ast with
  | ALTERNATIVE lst ->
      let  n  = List.length lst in
      let* a  = Choice.flipn n      in
      let elt = List.nth lst a  in
      begin match elt with 
      | Some t -> Choice.return t
      | None   -> Choice.fail
      end
  | _               -> Choice.fail 

let rec search_ast text pattern position =
  match pattern with
  | LITERAL     _ -> search_literal  text pattern position
  | SEQUENCE    _ -> search_sequence text pattern position
  | ALTERNATIVE _ -> search_alt      text pattern position
  | REPEATER    _ -> search_repeater text pattern position
  | _             -> Choice.fail (* Temp *)

and search_literal text pattern position =
  if position >= String.length text then Choice.fail
  else
    let ch = String.get text position in
    let (=) = Char.equal              in
    match pattern with
    | LITERAL (Some c) when c = ch -> Choice.return position
    | LITERAL None                 -> Choice.return position
    | _                            -> Choice.fail

and search_sequence text pattern position =
  let rec search_aux pattern position =
    match Ast.seq_get_elt pattern with
    | Some (h, tl) ->
        let* position = search_ast text h position in
        search_aux tl (position + 1)
    | None         ->
        Choice.return (position - 1)
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
        let* flip       = Choice.flipn 2 in
        begin match flip with
        | 0 -> if cond then Choice.return (position - 1) else Choice.fail
        | _ ->
          let* position   = search_ast text t position in
          search_aux (position + 1) (rep + 1)
        end
    | _ -> Choice.fail
  in
  search_aux position 0

let search text pattern =
  let rec search_aux pos xs =
    if pos >= String.length text then xs
    else
      let res = search_ast text pattern pos in
      match Sequence.max_elt res ~compare:(Int.compare) with
      | None                -> search_aux (pos + 1) xs
      | Some t when t < pos -> search_aux (pos + 1) ((-1, -1) :: xs)
      | Some t              -> search_aux (t + 1) ((pos, t) :: xs)
  in
  search_aux 0 []
