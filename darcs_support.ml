open Lwt
open Netstring_pcre
open Ocsforge_source_types
open Ocsforge_source_tree
open Xmltypes

(** Expression régulière utilisée pour darcs diff *)
let index_regexp = regexp "Index: "

(** Stocke le contenu d'un input channel dans une string list *)
let rec read_input_channel res chan =
  try_bind 
    (fun () -> Lwt_chan.input_line chan)
    (fun line -> read_input_channel (line::res) chan)
    (function 
      | End_of_file -> Lwt.return (List.rev res)
      | _ ->  Lwt.return (List.rev ("Unexpected Exception"::res)))
  
(** Execute une commande systeme et stockes le résultat affiché 
    dans une string list *)
let exec_command command error_message = 
  let (outpipe,inpipe) = Lwt_unix.pipe_in () in
  let pid = Unix.fork () in
  if (pid == 0) then begin
    Lwt_unix.close outpipe;
    Unix.dup2 inpipe Unix.stdout;
    Unix.close inpipe;
    Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |]
  end
  else begin
    Unix.close inpipe;
    (** A REMPLACER PAR LWT_UNIX WAIT_PID **)
    read_input_channel [] (Lwt_chan.in_channel_of_descr outpipe) >>= fun res ->
    match Unix.waitpid [] pid with
      | (_,Unix.WEXITED(i)) ->
	  if (i == 0) then begin
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

let rec eat_blanks s cpt =
  if (s.[cpt] != ' ') then
    string_after s cpt
  else
    eat_blanks s (cpt+1)

(** A partir d'une chaine de type rep1/.../repN/file, renvoie 
    un couple ([rep1;...;repN],file)*)
let format_name s = 
  let tmp = split (regexp "/") (eat_blanks s 0) in
  let name = List.nth (List.rev tmp) 0 in
  let path = List.rev (List.tl (List.rev tmp)) in
  (path,name)

let format_path p = 
  (split (regexp "/") p) 

(** Mise à jour de l'aborescence des fichiers d'un patch à partir 
    de l'arborescence du patch précédent et des changements effectués *)
(*
let insert_changes2 tree auth versionName versionID tree_changes = match tree_changes with        
| None -> Lwt.return tree
| Some(changes) ->
  let rec update_tree tree auth versionName versionID tree_changes = match tree_changes with
    | [] -> Lwt.return tree
    | h::t -> 
	begin match h with
        | Add_dir(d) -> 
	    format_name d >>= fun (path,name) ->
	      update_tree (insert (Dir(name,[])) path tree) auth versionName versionID t
	| Rm_dir(d) ->
	    format_name d >>= fun (path,name) ->
	      update_tree (delete (Dir(name,[])) path tree) auth versionName versionID t
	| Add_file(f) ->
	    format_name f >>= fun (path,name) ->
	      update_tree (insert (File(name,auth,(versionName,versionID))) path tree) 
		auth versionName versionID t
	| Rm_file(f) ->
	    format_name f >>= fun (path,name) ->
	      let node = get_node name path tree in
	      update_tree (delete node path tree) auth versionName versionID t
	| Move_file(oldF,newF) ->
	    format_name oldF >>= fun (oldPath,oldName) ->
	      format_name newF >>= fun (newPath,newName) ->
		update_tree (move oldPath oldName newPath newName tree) 
		  auth versionName versionID t
	| Modify_file(f) ->
	    format_name f >>= fun (path,name) ->
	      update_tree (update_infos path name auth versionName versionID tree) 
		auth versionName versionID t
      end
  in update_tree tree auth versionName versionID changes


let rec extract_patch_list2 res l = match l with
  | [] -> Lwt.return res 
  | h::t ->
      let oldTree = match res with
        | [] -> Dir(".",[])
	| _ -> (List.hd res).tree
      in
      (insert_changes2 oldTree h.xml_infos.xml_author h.xml_name h.xml_infos.xml_hash h.xml_tree_changes)
	>>= fun newTree ->
	let p = { id = ref(h.xml_infos.xml_hash);
	          name =  ref(h.xml_name);
		  author = ref(h.xml_infos.xml_author);
		  date  =  ref(h.xml_infos.xml_local_date);
		  comment =  ref(h.xml_comment);
		  tree = newTree }
	in
	extract_patch_list2 (p::res) t
*)


let rec handle_add_file tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with 
        | Simplexmlparser.PCData(s) -> 
	    let (path,name) = format_name s in
	    let new_tree = 
	      insert (File(name,!(patch.author),(!(patch.name),!(patch.id))))
		path tree in 
	    handle_add_file new_tree patch t
	| Simplexmlparser.Element(name,args,econtent) ->
	    handle_add_file tree patch t
	 

let rec handle_remove_file tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with 
        | Simplexmlparser.PCData(s) -> 
	    let (path,name) = format_name s in
	    let node = get_node name path patch.tree in
	    let new_tree = delete node path tree in
	    handle_remove_file new_tree patch t
	| Simplexmlparser.Element(name,args,econtent) ->
	    handle_remove_file tree patch t

let rec handle_add_directory tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with 
        | Simplexmlparser.PCData(s) -> 
	    let (path,name) = format_name s in
	    let new_tree = insert (Dir(name,[])) path tree in
	    handle_add_directory new_tree patch t
	| Simplexmlparser.Element(name,args,econtent) ->
	    handle_add_directory tree patch t


let rec handle_remove_directory tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with 
        | Simplexmlparser.PCData(s) -> 
	    let (path,name) = format_name s in
	    let new_tree = delete (Dir(name,[])) path tree in
	    handle_remove_directory new_tree patch t
	| Simplexmlparser.Element(name,args,econtent) ->
	    handle_remove_directory tree patch t


let handle_move tree patch args = match args with
  | [("from",mv_from);("to",mv_to)] ->
      let (from_path,from_name) = format_name mv_from in
      let (to_path,to_name) = format_name mv_to in
      move from_path from_name to_path to_name tree
  | _ -> tree 
  

let rec handle_summary res patch content = match content with
  | [] -> res
  | h::t -> 
      match h with
        | Simplexmlparser.PCData(_) -> handle_summary res patch t 
	| Simplexmlparser.Element(name,args,econtent) ->
	    let next_res = 
	      if (String.compare name "add_file" == 0) then
		handle_add_file res patch econtent
	      else if (String.compare name "remove_file" == 0) then
		handle_remove_file res patch econtent
	      else if (String.compare name "add_directory" == 0) then
		handle_add_directory res patch econtent
	      else if (String.compare name "remove_directory" == 0) then
		handle_remove_directory res patch econtent
	      else if (String.compare name "move" == 0) then 
		handle_move res patch args
	      else res
	    in
	    handle_summary next_res patch t

      

let handle_comment content = match content with
  | [] -> ""
  | h::_ -> 
      match h with
        | Simplexmlparser.PCData(comment) -> comment
	| _ -> ""

let handle_name content = match content with
  | [] -> ""
  | h::_ -> 
      match h with
        | Simplexmlparser.PCData(name) -> name
	| _ -> ""

let fill_patch_fields root_tree args = 
  let res = {id = ref "";
	     name = ref "";
	     author = ref "";
	     date = ref "";
	     comment = ref "";
	     tree = root_tree } 
  in
  let rec aux p args = match args with
    | [] -> p
    | (f,v)::t ->
	if (String.compare f "author" == 0) then
	  p.author := v
	else if (String.compare f "local_date" == 0) then
	  p.date := v
	else if (String.compare f "hash" == 0) then
	  p.id := v
	else ();
	aux p t
  in
  aux res args

let rec handle_patch ?file patch content = match content with
  | [] -> Lwt.return patch
  | h::t -> 
      match h with
        | Simplexmlparser.PCData(_) -> handle_patch patch t 
	| Simplexmlparser.Element(name,_,econtent) ->
	    if (String.compare name "name" == 0) then begin
	      patch.name := handle_name econtent;
	      handle_patch patch t 
	    end
	    else if (String.compare name "comment" == 0) then begin
	      patch.comment := handle_comment econtent;
	      handle_patch patch t 
	    end
	    else if (String.compare name "summary" == 0) then
	      handle_patch ({
			    id = patch.id;
			    name = patch.name;
			    author = patch.author;
			    date = patch.date;
			    comment = patch.comment;
			    tree = handle_summary 
			      (patch.tree) patch econtent;}) t
	    else handle_patch patch t 


let rec handle_changelog ?file res content = match content with 
  | [] -> Lwt.return res
  | p::t ->
      let root_tree = match res with
        | [] -> Dir(".",[])
	| _ -> (List.hd res).tree
      in
      match p with
        | Simplexmlparser.PCData(_) -> handle_changelog res t 
	| Simplexmlparser.Element(name,args,econtent) ->
	    if (String.compare name "patch" == 0) then
	      handle_patch (fill_patch_fields root_tree args) econtent >>=
	      fun patch ->
		handle_changelog (patch::res) t
	    else 
	      handle_changelog res t

(** Recupere la liste des patchs depuis le résultat de darcs changes *)
let rec extract_patch_list ?file res l = match l with
  | [] -> Lwt.return res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> extract_patch_list res t
	| Simplexmlparser.Element(name,_,content) ->
	    if (String.compare name "changelog" == 0) then
	      handle_changelog [] content
	    else
	      extract_patch_list res t
	      
	    

(** Stocke la liste des patchs présents dans un dépot Darcs dans une liste
    de types patch *)
let get_patch_list ?file rep =
  let command = match file with
  | None ->
      ("darcs changes --repodir "^rep
       ^" --xml-output --reverse --summary") 
  | Some(f) ->
      ("darcs changes --repodir "^rep
       ^" --xml-output --reverse --summary "^f) 
  in
  let error_message = "Error while getting changelog (signal received)" in
  exec_command command error_message >>= function
    | [] -> Lwt.return []
    | l -> 
	let s = String.concat "\n" l in
	let res = Simplexmlparser.xmlparser_string s in
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
			 newContent = currentDiff.newContent
			 @[(Diff,(string_after h 1))]}::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] == '-') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = currentDiff.oldContent
			 @[(Diff,(string_after h 1))];
			 newContent = 
			 currentDiff.newContent}::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] != '@') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = currentDiff.oldContent
			 @[(Common,(string_after h 1))];
			 newContent = currentDiff.newContent
			 @[(Common,(string_after h 1))]}::(List.tl parsed_res)
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
    parse_diff res [{fileName=file; oldContent=[]; newContent=[]}] false 
      >>= function 
	| [] -> Lwt.return {fileName=file; oldContent=[]; newContent=[]}
	| h::_ -> Lwt.return h 


let rec print_tree path tree = match tree with
  | File(f,a,(v,_)) -> 
      print_endline (path^f^"  aut:"^a^"   v:"^v)
  | Dir(d,l) ->
      print_endline (path^d);
      let rec aux list =  match list with
      | [] -> ()
      | h::t -> 
	  print_tree (path^d^"/") h;
	  aux t
      in aux l


let _ = 
  let darcs_fun_pack = 
    { vm_list = darcs_list;
      vm_cat = darcs_cat;
      vm_log = get_patch_list;
      vm_diff = darcs_diff } in
  Ocsforge_version_managers.set_fun_pack "Darcs" darcs_fun_pack

(** TEST **)
(*
let _ = 
  let path = "/home/jh/ocsigen.dev" in 
  print_endline "AVANT APPEL DARCS";
  (*darcs_diff "Makefile" path "20080524180748-0445d-376c7e08ca4606283fd4e1d9768137d40f804138.gz" "20080525110836-0445d-13dc6bca37acfd22c32f3b543d5b01616c7e007d.gz"*)
  (*darcs_cat ~patch:"0050804154654-e63a0-801e45ef2e1a81a413c975d8f507ee6613b11edf.gz" path "README"  *)
  darcs_list path >>= fun res ->(*
    List.iter (fun (_,p) -> print_endline p) res.oldContent;
    print_endline "********************************";
    List.iter (fun (_,p) -> print_endline p) res.newContent;*)
    print_endline "avant print_tree";
      print_tree "" res;
    Lwt.return (print_endline "FIN APPEL DARCS")
  *)   

