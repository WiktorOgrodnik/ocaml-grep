open Core
open Ast

let (let* ) = Bt.bind

let rec select a b =
  if   a >= b then Bt.fail
  else 
    let* c = Bt.flip in
    if c then Bt.return a
    else select (a + 1) b

let rec select_ast_element_from_alt ast =
  match ast with
  | ALTERNATIVE lst ->
      let  n = List.length lst  in
      let* a = select 0 (n - 1) in
      let elt = List.nth lst a  in
      begin match elt with 
      | Some t -> Bt.return t
      | None   -> Bt.fail
      end
  | _               -> Bt.fail 
;;

let triples n =
  let* a = select 1 n in
  let* b = select a n in
  let* c = select b n in
  if a*a + b*b = c*c then Bt.return (a, b, c)
  else Bt.fail

let rec is_matching text pattern postion =
  match pattern with
  | ALTERNATIVE _ ->
      let* pattern = select_ast_element pattern in
      is_matching text pattern postion
  | SEQUENCE    _ ->
      is_sequence_match 


let search text pattern =
  let rec search_aux pattern pos =
    
  search_aux pattern 0
  
