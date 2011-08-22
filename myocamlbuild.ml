(* OASIS_START *)
(* OASIS_STOP *)

let env =
  BaseEnvLight.load
    ~filename:MyOCamlbuildBase.env_filename
    ~allow_empty:true
    ()

let set_var var_name env_var =
  let s = BaseEnvLight.var_get var_name env in
  Unix.putenv env_var s

let set_var_option var_name env_var =
  match BaseEnvLight.var_get var_name env with
    | "none" -> ()
    | s -> Unix.putenv env_var s

let () =
  set_var "pgdatabase" "PGDATABASE";
  set_var "pgport" "PGPORT";
  set_var "pguser" "PGUSER";
  set_var "pgpassword" "PGPASSWORD";
  set_var_option "pghost" "PGHOST";
  set_var_option "pg_socket_domain_dir" "UNIX_SOCKET_DOMAIN_DIR"

open Ocamlbuild_plugin

let patch_lexer _ _ =
  Cmd (S [ A "patch";
	   A "-o"; P "src/ocaml_lexer.mll";
	   P "ocaml/parsing/lexer.mll";
	   P "src/ocaml_lexer.patch" ])

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
	 | After_rules ->
	   rule "patch-lexer"
	     ~prod:"src/ocaml_lexer.mll"
	     ~deps:["src/ocaml_lexer.patch"; "ocaml/parsing/lexer.mll"]
	     patch_lexer
	 | _ -> ()
    )
