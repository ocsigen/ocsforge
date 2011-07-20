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

let () = Ocamlbuild_plugin.dispatch dispatch_default;;
