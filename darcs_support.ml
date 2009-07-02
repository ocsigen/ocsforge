open Lwt
open Netstring_str
open Ocsforge_source_types
open Ocsforge_source_tree
open Xmltypes

(** Expression régulière utilisée pour darcs diff *)
let index_regexp = regexp "Index: "


(** Stocke le contenu d'un input channel dans une string list *)
let rec read_input_channel res chan =
  catch
    (fun () -> Lwt_chan.input_line chan >>= fun line ->
               read_input_channel (res@[line]) chan)
    (function 
      | End_of_file -> Lwt.return res
      | _ ->  Lwt.return ("Unexpected Exception"::res))


(** Execute une commande systeme et stockes le résultat affiché 
    dans une string list *)
let exec_command command error_message = 
  let (outpipe,inpipe) = Lwt_unix.pipe_in () in
  let pid = Unix.fork () in
  if (pid == 0) then begin
    Lwt_unix.close outpipe;
    Unix.dup2 (inpipe) (Unix.stdout);
    Lwt_unix.system command >>= function
      | Unix.WEXITED(_) ->
	  Unix.close inpipe;
	  exit(0)
      | Unix.WSIGNALED(_) 
      | Unix.WSTOPPED(_) -> 
	  Unix.close inpipe;
	  exit(1)
  end
  else begin
    Unix.close inpipe;
    Lwt_unix.wait () >>= function
      | (_,Unix.WEXITED(i)) ->
	  if (i == 0) then begin
	    read_input_channel [] (Lwt_chan.in_channel_of_descr outpipe) >>= fun res ->
	      Lwt_unix.close outpipe;
	      Lwt.return res
	  end
	  else begin
	    Lwt_unix.close outpipe;
	    Lwt.return [error_message]
	  end
      | (_,_) -> 
	  Lwt_unix.close outpipe;
	  Lwt.return [error_message]
  end

(** Recompose la chaine correspondant au résultat d'une commande *)
let rec to_string l s = match l with
  | [] -> Lwt.return s
  | h::t -> to_string t (s^"\n"^h)

(** A partir d'une chaine de type rep1/.../repN/file, renvoie 
    un couple ([rep1;...;repN],file)*)
let format_name s = 
  let tmp = split (regexp "/") s in
  let name = List.nth (List.rev tmp) 0 in
  let path = List.rev (List.tl (List.rev tmp)) in
  Lwt.return (path,name)

(** Mise à jour de l'aborescence des fichiers d'un patch à partir 
    de l'arborescence du patch précédent et des changements effectués *)
let rec update_tree tree auth version tree_changes = match tree_changes with
  | [] -> Lwt.return tree
  | h::t -> 
      begin match h with
        | Add_dir(d) -> 
	    format_name d >>= fun (path,name) ->
	      update_tree (insert (Dir(name,[])) path tree) auth version t
	| Rm_dir(d) ->
	    format_name d >>= fun (path,name) ->
	      update_tree (delete (Dir(name,[])) path tree) auth version t
	| Add_file(f) ->
	    format_name f >>= fun (path,name) ->
	      update_tree (insert (File(name,auth,version)) path tree) auth version t
	| Rm_file(f) ->
	    format_name f >>= fun (path,name) ->
	      let node = get_node name path tree in
	      update_tree (delete node path tree) auth version t
	| Move_file(oldF,newF) ->
	    format_name oldF >>= fun (oldPath,oldName) ->
	      format_name newF >>= fun (newPath,newName) ->
		update_tree (move oldPath oldName newPath newName tree) auth version t
	| Modify_file(f) ->
	    format_name f >>= fun (path,name) ->
	      update_tree (update_infos path name auth version tree) auth version t
      end

(** Recupere la liste des patchs depuis le résultat de darcs changes *)
let rec extract_patch_list res l = match l with
  | [] -> Lwt.return res 
  | h::t ->
      let oldTree = match res with
        | [] -> Dir(".",[])
	| _ -> (List.hd res).tree
      in
      (update_tree oldTree h.xml_infos.xml_author h.xml_name h.xml_tree_changes) >>= fun newTree ->
	let p = { id = ref(h.xml_infos.xml_hash);
	          name =  ref(h.xml_name);
		  author = ref(h.xml_infos.xml_author);
		  date  =  ref(h.xml_infos.xml_local_date);
		  comment =  ref(h.xml_comment);
		  tree = newTree }
	in
	extract_patch_list (p::res) t

(** Stocke la liste des patchs présents dans un dépot Darcs dans une liste
    de types patch *)
let get_patch_list rep =
  let command = ("darcs changes --repodir "^rep^" --xml-output --reverse --summary") in
  let error_message = "Error while getting changelog (signal received)" in
  exec_command command error_message >>= function
    | [] -> Lwt.return []
    | l -> 
	to_string l "" >>= fun s ->
	  let lexbuf = Lexing.from_string s in
	  let res = Xml_parser.log Xml_lexer.token lexbuf in
	  extract_patch_list [] res
    
(** Recuperes l'arbre associé au patch précisé *)
let darcs_list ?id rep = 
  get_patch_list rep >>= fun pl -> match id with
    | None ->  
	begin match pl with
	| [] -> Lwt.return (Dir("",[])) (* a traiter *)
	| _ -> Lwt.return ((List.hd pl).tree)
	end
    | Some(hash) ->
	let rec aux l = match l with
        | [] -> Lwt.return (Dir("",[])) (* a traiter *)
	| p::t -> 
	    if ((String.compare hash !(p.id)) == 0) then
	      Lwt.return (p.tree)
	    else aux t
	in aux pl

(** Stocke le contenu d'un fichier d'un dépôt Darcs dans une string list *)
let darcs_cat ?id rep file = 
  let error_message = "Error while getting file content (signal received)" in
  match id with
    | None -> 
	let command = ("darcs show contents --repodir "^rep^" "^file) in 
	exec_command command error_message
    | Some(matching) ->
	let command = ("darcs show contents --match 'hash "^matching^
		       "' --repodir "^rep^
		       " "^file) in
	exec_command command error_message
  

(** Parse le résultat de darcs diff pour stocker son contenu dans 
    une liste de types file_diff *)
let rec parse_diff diff_res parsed_res started = match diff_res with
  | [] -> Lwt.return parsed_res
  | h::t ->  
      let currentDiff = List.hd parsed_res in
      if 
	(String.length h > 2 &&
	 ((h.[0] == '-' && h.[1] == '-' && h.[2] == '-')
       || (h.[0] == '+' && h.[1] == '+' && h.[2] == '+'))) then
	parse_diff t parsed_res true
      else if (started == false) then
	parse_diff t parsed_res started
      else begin
	if (String.length h == 0) then parse_diff t parsed_res started
	else if (h.[0] == '+') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = currentDiff.oldContent;
			 newContent = currentDiff.newContent@[(Diff,(string_after h 1))]}::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] == '-') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = currentDiff.oldContent@[(Diff,(string_after h 1))];
			 newContent = currentDiff.newContent}::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] != '@') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = currentDiff.oldContent@[(Common,(string_after h 1))];
			 newContent = currentDiff.newContent@[(Common,(string_after h 1))]}::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else parse_diff t parsed_res started
      end
     


(** Execute la commande darcs diff et applique parse_diff sur le résultat *)
let darcs_diff file rep from_patch to_patch = 
  let error_message = "Error while getting diff result (signal received)" in
  (*match file with
  | None ->
      let command = ("darcs diff --from-match 'hash "^from_patch^
		     "' --to-match 'hash "^to_patch^
		     "' -u --repodir "^rep) in
      exec_command command error_message >>= fun res ->
	(parse_diff res []) 
    | Some(filename) ->*)
  let command = ("darcs diff --from-match 'hash "^from_patch^
		 "' --to-match 'hash "^to_patch^
		 "' "^file^
		 " -u --repodir "^rep) in
  exec_command command error_message >>= fun res ->
    parse_diff res [{fileName=file; oldContent=[]; newContent=[]}] false >>= function 
      | [] -> Lwt.return {fileName=file; oldContent=[]; newContent=[]}
      | h::_ -> Lwt.return h 

let _ = 
  let darcs_fun_pack = 
    { vm_list = darcs_list;
      vm_cat = darcs_cat;
      vm_log = get_patch_list;
      vm_diff = darcs_diff } in
  Ocsforge_version_managers.set_fun_pack "Darcs" darcs_fun_pack

(*  
  let path = "/home/jh/lwt" in 
  let file = "README" in
  
  print_endline "AVANT APPEL";
  darcs_diff "Makefile" path "20080524180748-0445d-376c7e08ca4606283fd4e1d9768137d40f804138.gz" "20080525110836-0445d-13dc6bca37acfd22c32f3b543d5b01616c7e007d.gz"
  (*darcs_cat ~patch:"0050804154654-e63a0-801e45ef2e1a81a413c975d8f507ee6613b11edf.gz" path file  *)
  (*get_patch_list path*) >>= fun res ->
    List.iter (fun (_,p) -> print_endline p) res.oldContent;
    print_endline "********************************";
    List.iter (fun (_,p) -> print_endline p) res.newContent;
    Lwt.return (print_endline "FIN")
      
      *)
