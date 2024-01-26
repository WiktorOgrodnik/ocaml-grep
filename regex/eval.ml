open Core
open Ast

let (let* ) = Choice.bind

type state =
  { text  : string
  ; logic : bool }

let select_from_alt ast =
  match ast with
  | ALTERNATIVE lst ->
      let  n  = List.length lst in
      let* a  = Choice.flipn n  in
      let elt = List.nth lst a  in
      begin match elt with 
      | Some t -> Choice.return t
      | None   -> Choice.fail
      end
  | _               -> Choice.fail

let rec search_success state pos =
  match state.logic with
  | true  -> Choice.return pos
  | false -> Choice.fail

and search_failure state pos =
  search_success { state with logic = not state.logic } pos

and search_ast pattern state pos =
  match pattern with
  | LITERAL     _ -> search_literal  pattern state pos
  | SEQUENCE    _ -> search_sequence pattern state pos 
  | ALTERNATIVE _ -> search_alt      pattern state pos
  | REPEATER    _ -> search_repeater pattern state pos
  | NEGATION    _ -> search_negation pattern state pos
  | INVALID       -> Choice.fail (* always fail - Invalid *)

and search_literal pattern state pos =
  let in_bounds chars =
    let ints = List.map chars ~f:Char.to_int in
    List.is_sorted ints ~compare:(fun a b -> a - b)
  in
  if pos >= String.length state.text then Choice.fail 
  else
    let ch = String.get state.text pos in
    let (=) = Char.equal              in
    match pattern with
    | LITERAL (RANGE (a, b)) when in_bounds [a; ch; b] -> search_success state pos
    | LITERAL (SINGLE c) when c = ch                   -> search_success state pos
    | LITERAL ANY                                      -> search_success state pos
    | _                                                -> search_failure state pos

and search_sequence ?(modifier = 1) pattern state pos =
  let rec search_aux pattern pos =
    match Ast.seq_get_elt pattern with
    | Some (h, tl) ->
        let* pos = search_ast h state pos in
        search_aux tl (pos + modifier)
    | None         ->
        Choice.return (pos - modifier)
  in
  search_aux pattern pos

and search_alt pattern state pos =
  if state.logic then
    let* pattern = select_from_alt pattern in
    search_ast pattern state pos
  else
    search_sequence ~modifier:0 pattern state pos

and search_repeater pattern state pos =
  let some_or v o =
    match v with
    | Some v -> v
    | None   -> o
  in
  let rec search_aux pos rep =
    match pattern with
    | REPEATER (t, r) ->
        let left_bound  = some_or r.l 0                            in
        let right_bound = some_or r.r Int.max_value                in
        let cond        = rep >= left_bound && rep <= right_bound  in
        let* flip       = Choice.flipn 2 in
        begin match flip with
        | 0 -> if cond then search_success state (pos - 1) else search_failure state (pos - 1)
        | _ ->
          let* pos   = search_ast t state pos in
          search_aux (pos + 1) (rep + 1)
        end
    | _ -> Choice.fail
  in
  search_aux pos 0

and search_negation pattern state pos =
  match pattern with
  | NEGATION t -> search_ast t { state with logic = not state.logic } pos
  | _          -> Choice.fail

let search text pattern =
  let rec search_aux pos xs =
    if pos >= String.length text then xs
    else
      let res = search_ast pattern { text = text; logic = true } pos in
      match Sequence.max_elt res ~compare:(Int.compare) with
      | None                -> search_aux (pos + 1) xs
      | Some t when t < pos -> search_aux (pos + 1) ((-1, -1) :: xs)
      | Some t              -> search_aux (t + 1) ((pos, t) :: xs)
  in
  search_aux 0 []
