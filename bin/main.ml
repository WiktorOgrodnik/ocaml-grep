let print_line line =
  print_endline line
;;

let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " [options] -F file_path -P pattern" 
let show_help = ref false
let file_path = ref ""
let pattern   = ref ""

let speclist = 
  [("--help", Arg.Set        show_help, "Help msg");
  (("-F",     Arg.Set_string file_path, "File path to an examine file"));
  (("-P",     Arg.Set_string pattern,   "Regex pattern"));]

let grep_main fp process pattern =
  let chn = open_in fp in
  let rec rl () =
    try
      let line = input_line chn in
      Regex.search line process pattern;
      print_endline "STOP!";
      rl ()
    with
    | End_of_file -> close_in chn
  in rl ()
;;

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;

  if !show_help then begin
    print_endline usage_msg;
    exit 0;
  end;

  grep_main !file_path print_line !pattern

;;
