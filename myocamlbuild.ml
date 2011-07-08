(* OASIS_START *)
(* OASIS_STOP *)

Unix.putenv "PGDATABASE" "ocsimore";;

Ocamlbuild_plugin.dispatch dispatch_default;;
