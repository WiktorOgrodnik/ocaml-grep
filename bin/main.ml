open Core
open ANSITerminal

type ftype =
| File   of string
| Stream of In_channel.t

let print_line line locations =
  let idx_in idx xs =
    List.fold 
      (List.map xs ~f:(fun (l, r) -> l <= idx && idx <= r)) 
    ~init:false ~f:(||)
  in  
  let rec print_char_aux idx linex =
    match linex with
    | c :: cx ->
        let styles = if idx_in idx locations 
          then [Foreground Red]
          else [Foreground Default]
        in
        print_string styles (String.make 1 c);
        print_char_aux (idx + 1) cx 
    | []      -> print_endline ""
  in
  print_char_aux 0 (String.to_list line)

let usage_msg = "Usage: " ^ Array.get (Sys.get_argv ()) 0 ^ " [OPTION]... PATTERNS [FILE]..." 
let show_help = ref false

let speclist = 
  [("--help", Arg.Set        show_help, "Help msg")]

let show_help_msg () =
  print_endline usage_msg

let grep_main chn process pattern =
  let rec rl () =
    match In_channel.input_line chn with
    | Some line -> 
      Regex.search line process pattern;
      rl ()
    | None      -> In_channel.close chn
  in rl ()

let get_pattern () =
  try
    Some (Array.get (Sys.get_argv ()) 1)
  with
  | Invalid_argument _ -> show_help := true; None

let get_files () =
  let files = Array.to_list (Sys.get_argv ())       in
  List.drop files 2

let () =
  let pattern = get_pattern () in
  let files   = get_files   () in
  let files_m = if List.is_empty files 
    then [Stream In_channel.stdin]
    else List.map files ~f:(fun a -> File a)
  in
  let rec main_aux pattern files =
    match files with
    | f :: xf ->
      begin match f with
        | File f     ->
          begin try
            let chn = In_channel.create f in
            grep_main chn print_line pattern;
            main_aux pattern xf
          with
          | Sys_error _ -> print_endline ("Cannot open file: " ^ f)
          end
        | Stream chn ->
          grep_main chn print_line pattern;
          main_aux pattern xf
      end
    | [] -> ()
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;

  if !show_help then begin
    show_help_msg (); exit 0;
  end;

  match pattern with
  | None -> ()
  | Some pattern ->
      begin match Regex.compile pattern with
      | Ok pattern   -> main_aux pattern files_m
      | Error err    -> print_endline (Error.to_string_hum err)
      end
