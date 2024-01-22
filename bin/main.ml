open Core
open ANSITerminal

(* let print_locations locations =
  let rec pr_aux loc =
    match loc with
    | c :: cx ->
      print_string [] ("(" ^ (string_of_int (fst c)) ^ ", " ^ (string_of_int (snd c)) ^ ") ");
      pr_aux cx
    | [] -> ()
  in
  pr_aux locations;
  print_endline "" *)

let print_line line locations =
  let idx_in idx xs =
    List.fold 
      (List.map xs ~f:(fun (l, r) -> l <= idx && idx <= r)) 
    ~init:false ~f:(||)
  in  
  let rec print_char_aux idx linex =
    match linex with
    | c :: cx ->
        let styles =
          if (idx_in idx locations) then 
            [Foreground Red]
          else 
            [Foreground Default]
        in
        print_string styles (String.make 1 c);
        print_char_aux (idx + 1) cx 
    | []      -> print_endline ""
  in
  print_char_aux 0 (String.to_list line)

let usage_msg = "Usage: " ^ " [options] -F file_path -P pattern" 
let show_help = ref false
let file_path = ref ""
let pattern   = ref ""

let speclist = 
  [("--help", Arg.Set        show_help, "Help msg");
  (("-F",     Arg.Set_string file_path, "File path to an examine file"));
  (("-P",     Arg.Set_string pattern,   "Regex pattern"));]

let grep_main fp process pattern =
  let chn = In_channel.create fp in
  let rec rl () =
    match In_channel.input_line chn with
    | Some line -> Regex.search line process pattern; rl ()
    | None      -> ()
  in rl ()

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  if !show_help then begin
    print_endline usage_msg;
    exit 0;
  end;

  grep_main !file_path print_line !pattern
