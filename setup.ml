(* OASIS_START *)

let () =
  let command = Printf.sprintf "oasis setup-dev -run %s %s" Sys.executable_name (String.concat " " (Array.to_list Sys.argv)) in
  Printf.eprintf "I: Running command '%s'\n%!" command;
  exit (Sys.command command)
;;

(* OASIS_STOP *)

let () = setup ();;
