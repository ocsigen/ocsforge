(* Ocsimore
 * Copyright (C) 2009
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** @author Granarolo Jean-Henri *)

open Lwt
open Netstring_pcre
open Ocsforge_source_types
open Ocsforge_source_tree
open Xmltypes

module Vm = Ocsforge_version_managers

(** Expression régulière utilisée pour darcs diff *)
let index_regexp = regexp "Index: "

(** Expression régulière utilisée pour darcs changes *)
let ignore_this = regexp "(Ignore-this: )[a-z 0-9]+"


(** Stocke le contenu d'un input channel dans une string list *)
let rec read_input_channel buf res chan =
  Lwt_chan.input chan buf 0 4096 >>= fun n ->
    if (n = 0) then begin
      Lwt_chan.close_in chan >>= fun () ->
        Lwt.return (Buffer.contents res)
    end
    else begin
      Buffer.add_substring res buf 0 n;
      read_input_channel buf res chan
    end


(** Execute une commande systeme et stockes le résultat affiché
    dans une string list *)
let exec_command command error_message =
  let open Lwt_process in
  let recv_status (pr:process_in) =
    let ic = pr#stdout in
    lwt s =
      try_lwt
        Lwt_io.read ic
      finally
        Lwt_io.close ic >> return ()
    in
    lwt status = pr#status in
    return (s,status)
  in
  match_lwt recv_status (open_process_in ("/bin/sh",[| "/bin/sh"; "-c"; command |])) with
    | res,Unix.WEXITED(0) ->
      Lwt.return res
    | (_,Unix.WEXITED(_)) ->
      Lwt.fail Ocsforge_version_managers.Manager_command_error
    | (_,_) ->
      Lwt.return error_message

let get_nb_patches rep =
  let command = ("darcs changes --repodir "^rep^" --count") in
  let error_message = "Error while getting total patches number (signal received)" in
  exec_command command error_message >>= fun s ->
    Lwt.return (int_of_string (String.sub s 0 (String.length s - 1)))


let get_patch_num hash rep =
  let command =
    ("darcs changes --repodir "^rep^" --to-match 'hash "
     ^hash^"' --count") in
  let error_message = "Error while getting patch number (signal received)" in
  exec_command command error_message >>= fun s ->
    Lwt.return (int_of_string (String.sub s 0 (String.length s - 1)))


let get_patch_index hash rep =
  get_nb_patches rep >>= fun max ->
    get_patch_num hash rep >>= fun nb ->
      Lwt.return (max - nb + 1)

let rec eat_blanks s cpt =
  if (s.[cpt] != ' ') then
    string_after s cpt
  else
    eat_blanks s (cpt+1)

(** A partir d'une chaine de type rep1/.../repN/file, renvoie
    un couple ([rep1;...;repN],file)*)
let format_name s ~eatblanks =
  let tmp =
  if (eatblanks) then
    split (regexp "/") (eat_blanks s 0)
  else
    split (regexp "/") s in
  let name = List.nth (List.rev tmp) 0 in
  let path = List.rev (List.tl (List.rev tmp)) in
  (path,name)

let format_path p =
  (split (regexp "/") p)

let rec handle_add_file tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with
        | Simplexmlparser.PCData(s) ->
	    let (path,name) = format_name s ~eatblanks:true in
	    let new_tree =
	      insert (File(name,!(patch.author),(!(patch.name),!(patch.id))))
		path tree in
	    handle_add_file new_tree patch t
	| _ ->
	    handle_add_file tree patch t


let rec handle_remove_file tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with
        | Simplexmlparser.PCData(s) ->
	    let (path,name) = format_name s ~eatblanks:true in
	    let node = get_node name path patch.tree in begin
	      match node with
	        | None -> handle_remove_file tree patch t
		| Some(n) ->
		    let new_tree = delete n path tree in
		    handle_remove_file new_tree patch t
	    end
	| _ ->
	    handle_remove_file tree patch t

let rec handle_add_directory tree patch content = match content with
  | [] -> tree
  | h::t ->
        match h with
        | Simplexmlparser.PCData(s) ->
	    let (path,name) = format_name s ~eatblanks:true in
	    let new_tree = insert (Dir(name,[])) path tree in
	    handle_add_directory new_tree patch t
	| _ ->
	    handle_add_directory tree patch t


let rec handle_remove_directory tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with
        | Simplexmlparser.PCData(s) ->
	    let (path,name) = format_name s ~eatblanks:true in
	    let new_tree = delete (Dir(name,[])) path tree in
	    handle_remove_directory new_tree patch t
	| _ ->
	    handle_remove_directory tree patch t

let handle_move tree args = match args with
  | [("from",mv_from);("to",mv_to)] ->
      let (from_path,from_name) = format_name mv_from ~eatblanks:false in
      let (to_path,to_name) = format_name mv_to ~eatblanks:false in
      move from_path from_name to_path to_name tree
  | _ -> tree

let rec handle_modify_file tree patch content = match content with
  | [] -> tree
  | h::t ->
      match h with
      | Simplexmlparser.PCData(s) ->
	  let tmp = split (regexp "\n") s in
	  let (path,name) = format_name (List.nth tmp 0) ~eatblanks:true in
	  handle_modify_file (update_infos path name
				!(patch.author)
				!(patch.name)
				!(patch.id) tree) patch t
      | _ -> handle_modify_file tree patch t


let rec handle_summary res patch content = match content with
  | [] -> res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_summary res patch t
	| Simplexmlparser.Element(name,args,econtent) ->
	    let next_res =
	      if (name = "add_file") then
		handle_add_file res patch econtent
	      else if (name = "remove_file") then
		handle_remove_file res patch econtent
	      else if (name = "add_directory") then
		handle_add_directory res patch econtent
	      else if (name = "remove_directory") then
		handle_remove_directory res patch econtent
	      else if (name = "modify_file") then
		handle_modify_file res patch econtent
	      else if (name = "move") then
		handle_move res args
	      else res
	      in
	      handle_summary next_res patch t



let rec handle_comment res content = match content with
  | [] -> res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(comment) ->
            let parsed_comment = split ignore_this comment
            in
            let c =
              if (List.length parsed_comment > 0) then
                List.hd parsed_comment
              else ""
            in
            handle_comment (res^c) t
	| _ -> handle_comment res t

let rec handle_name res content = match content with
  | [] -> res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(name) -> handle_name (res^name) t
	| _ -> handle_name res t

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
	if (f = "author") then
	  let parsed_author = split (regexp "&lt") v in
	  p.author := List.hd (parsed_author)
	else if (f = "local_date") then
	  p.date := v
	else if (f = "hash") then
	  p.id := v
	else ();
	aux p t
  in
  aux res args

let rec handle_patch patch content = match content with
  | [] -> Lwt.return patch
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_patch patch t
	| Simplexmlparser.Element(name,_,econtent) ->
	    if (name = "name") then begin
	      patch.name := handle_name "" econtent;
	      handle_patch patch t
	    end
	    else if (name = "comment") then begin
	      patch.comment := handle_comment "" econtent;
	      handle_patch patch t
	    end
	    else if (name = "summary") then
	      handle_patch ({
			    id = patch.id;
			    name = patch.name;
			    author = patch.author;
			    date = patch.date;
			    comment = patch.comment;
			    tree = handle_summary
			      (patch.tree) patch econtent;}) t
	    else handle_patch patch t


let rec handle_changelog res content = match content with
  | [] -> Lwt.return res
  | p::t ->
      let root_tree = match res with
        | [] -> Dir(".",[])
	| _ -> (List.hd res).tree
      in
      match p with
        | Simplexmlparser.PCData(_) -> handle_changelog res t
	| Simplexmlparser.Element(name,args,econtent) ->
	    if (name = "patch") then
	      handle_patch (fill_patch_fields root_tree args) econtent >>=
	      fun patch ->
		handle_changelog (patch::res) t
	    else
	      handle_changelog res t

(** Recupere la liste des patchs depuis le résultat de darcs changes *)
(** /!\ UTILISER FILE LORS D'UN LOG SUR UN FICHIER POUR NE PAS CONSTRUIRE D'ARBRE INUTILE /!\ **)
let rec handle_log_xml_list res l = match l with
  | [] -> Lwt.return res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_log_xml_list res t
	| Simplexmlparser.Element(name,_,content) ->
	    if (name = "changelog") then begin
              handle_changelog res content >>= fun new_res ->
	      handle_log_xml_list new_res t
            end
	    else
	      handle_log_xml_list res t


let rec handle_annot_patch args = match args with
  | [] -> ""
  | (f,v)::t ->
      if (f = "author") then
        let parsed_author = split (regexp "&lt") v in
	List.hd (parsed_author)
      else
        handle_annot_patch t


let rec handle_added_by l = match l with
  | [] -> ""
  | h::t ->
      match h with
      | Simplexmlparser.PCData(_) ->
          handle_added_by t
      | Simplexmlparser.Element(name,args,_) ->
          if (name = "patch") then
            handle_annot_patch args
          else
            handle_added_by t


let rec handle_line aut line_content l = match l with
  | [] -> Lwt.return ((aut^"\n"),(line_content^"\n"))
  | h::t ->
      match h with
        | Simplexmlparser.PCData(line) ->
            handle_line aut (line_content^line) t
        | Simplexmlparser.Element(name,_,content) ->
            if (name = "added_by") then
              handle_line (handle_added_by content) line_content t
            else handle_line aut line_content t


let rec handle_modified l = match l with
  | [] -> ""
  | h::t ->
      match h with
      | Simplexmlparser.Element(name,args,_) ->
          if (name = "patch") then
            handle_annot_patch args
          else
            handle_modified t
      | _ ->
          handle_modified t


let rec handle_file res aut l = match l with
  | [] -> Lwt.return res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_file res aut t
        | Simplexmlparser.Element(name,_,content) ->
            if (name = "normal_line") then
              handle_line "" "" content >>= fun line ->
                if (fst line = "\n") then
                  handle_file res aut t
                else
                handle_file (line::res) aut t
            else if (name = "modified") then
              handle_file res (handle_modified content) t
            else if (name = "added_line") then
              handle_line aut "" content >>= fun line ->
                handle_file (line::res) aut t
            else
              handle_file res aut t

(** Recupere la liste (auteur,ligne) depuis le résultat darcs annotate *)
let rec handle_annot_xml_list res l = match l with
  | [] -> Lwt.return res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_annot_xml_list res t
        | Simplexmlparser.Element(name,_,content) ->
            if (name = "file") then
              handle_file [] "" content
            else
              handle_annot_xml_list res t


(** Stocke la liste des patchs présents dans un dépot Darcs ainsi que les
    modifications faites sur les fichiers dans une liste de types patch *)
let get_patch_list ?id ?dir repo =
  let command = match (id,dir) with
  | (None,None) ->
      ("darcs changes --repo "^repo
       ^" --xml-output --summary --reverse")
  | (None,Some(d)) ->
      ("darcs changes --repo "^repo^" --repodir "^d
       ^" --xml-output --summary --reverse")
  | (Some(matching),None) ->
       ("darcs changes --repo "^repo
       ^" --to-match 'hash "^matching^"' --xml-output --summary --reverse")
  | (Some(matching),Some(d)) ->
      ("darcs changes --repo "^repo^" --repodir "^d
       ^" --to-match 'hash "^matching^"' --xml-output --summary --reverse")
  in let error_message = "Error while getting changelog (signal received)" in
  exec_command command error_message >>= fun s ->
    let res = Simplexmlparser.xmlparser_string s in
    handle_log_xml_list [] res



let darcs_log ?file ?range ?limit rep =
  let lwt_index = match (range,limit) with
  | (Some(Some(end_rev),None),Some(l)) ->
      get_patch_index end_rev rep >>= fun num ->
          Lwt.return ("-n 1-"^(string_of_int (num+l)))
  | (Some(None,Some(start_rev)),_) ->
      Lwt.return ("--to-match 'hash "^start_rev^"'")
  | (Some(Some(start_rev),Some(end_rev)),Some(l)) ->
      get_patch_index start_rev rep >>= fun start ->
        get_patch_index end_rev rep >>= fun er ->
          let sr = max (start-l) 1 in
          Lwt.return ("-n "^(string_of_int sr)^"-"^(string_of_int (er+l)))
  | _ -> Lwt.return ""
  in
  lwt_index >>= fun index ->
  let command = match file with
  | None ->
      begin match limit with
      | None -> ("darcs changes --repodir "^rep
	         ^" "^index^" --xml-output --reverse")
      | Some(i) -> ("darcs changes --repodir "^rep
	            ^" "^index^" --xml-output --reverse --max-count "^(string_of_int (3*i)))
      end
  | Some(f) ->
      begin match limit with
      | None ->("darcs changes --repodir "^rep
	        ^" "^index^"--xml-output --reverse "^f)
      | Some(i) ->
          ("darcs changes --repodir "^rep
	   ^" "^index^" --xml-output --max-count "^(string_of_int (3*i))
           ^" --reverse "^f)
      end
  in let error_message = "Error while getting changelog (signal received)" in
  exec_command command error_message >>= fun s ->
    let res = Simplexmlparser.xmlparser_string s in
    handle_log_xml_list [] res


(** Récupères les informations relatives à un fichier de la copie de travail *)
let darcs_infos repos_path file =
  let error_message = "Error while getting file info (signal received)" in
  let command =
    ("darcs changes --xml-output --repodir "^repos_path
     ^" --max-count=1 "^file)
  in exec_command command error_message >>= fun _ -> Lwt.return ()



(** Construit un arbre de fichiers à partir du résultat de darcs show files *)
let parse_wc_list ?dir dir_list file_list =
  let insert_dirs = match dir_list with
    | [] -> Dir("",[])
    | _ ->
        let rec insert_dir_aux l res = match l with
          | [] -> res
          | h::t ->
              if (String.compare h "." = 0) then
                insert_dir_aux t res
              else begin
                let (path,name) = format_name (String.sub h 2 ((String.length h) - 2)) ~eatblanks:false
                in
                let new_tree = insert (Dir(name,[])) path res in
                insert_dir_aux t new_tree
              end
        in insert_dir_aux dir_list (Dir("",[]))
  in
  let rec insert_files l res = match l with
  | [] -> Lwt.return res
  | h::t ->
      let parsed_name = String.sub h 2 ((String.length h) - 2) in
      let (path,name) = format_name parsed_name ~eatblanks:false
      in
      begin match (dir,path) with
      | (None,[]) ->
          (*darcs_infos repos_path parsed_name >>= fun () ->*)
          let new_tree = insert (File(name,"",("",""))) path res in
          insert_files t new_tree
      | (Some(d),_) ->
          if (d = (String.concat "/" path)) then begin
            (*darcs_infos repos_path parsed_name >>= fun () ->*)
            let new_tree = insert (File(name,"",("",""))) path res in
            insert_files t new_tree
          end
          else
            let new_tree = insert (File(name,"",("",""))) path res in
            insert_files t new_tree
      | _ ->
          let new_tree = insert (File(name,"",("",""))) path res in
          insert_files t new_tree
      end
  in insert_files file_list insert_dirs
(*
  let rec insert_files = match l with
  | [] -> res
  | h::t -> *)


(** Crées l'arbre associé a la copie de travail *)
let darcs_wc_list ?dir repos_path =
  let error_message = "Error while getting content list (signal received)" in
  let dir_command = ("darcs show files --no-files --repodir "^repos_path) in
  let file_command = ("darcs show files --no-directories --repodir "^repos_path) in
  exec_command dir_command error_message >>= fun dir_string ->
    let dir_list = split (regexp "\n") dir_string in
    exec_command file_command error_message >>= fun file_string ->
      let file_list = split (regexp "\n") file_string in
      match dir with
      | None ->
          parse_wc_list dir_list file_list
      | Some(d) ->
          parse_wc_list ~dir:d dir_list file_list >>= fun tree ->
            let (target,name) = format_name d ~eatblanks:false in
            let res = get_node name target tree in match res with
              | None -> Lwt.fail Vm.Node_not_found
              | Some(t) ->
                  match t with
                  | Dir(_) -> Lwt.return t
                  | _ -> Lwt.fail Vm.Wrong_node_kind


(** Recuperes l'arbre associé au patch précisé *)
let darcs_list ?id ?dir repos_path =
  let rec find_rev rev l = match l with
    | [] -> Lwt.fail Vm.Revision_not_found
    | p::t ->
        if ((String.compare rev !(p.id)) == 0) then
	  Lwt.return (p.tree)
        else find_rev rev t
  in
  match (id,dir) with
    | (None,None) ->
        darcs_wc_list repos_path;
    | (Some(i),None) ->
        get_patch_list ~id:i repos_path >>= fun l -> begin match l with
          | [] -> Lwt.fail Vm.Revision_not_found
          | _ -> find_rev i l
        end
    | (None,Some(d)) ->
        darcs_wc_list ~dir:d repos_path
    | (Some(i),Some(d)) ->
        get_patch_list ~id:i ~dir:d repos_path >>= fun l -> begin match l with
          | [] -> Lwt.fail Vm.Revision_not_found
          | _ ->
              find_rev i l >>= fun tree ->
                let (target,name) = format_name d ~eatblanks:false in
                let res = get_node name target tree in match res with
                  | None -> Lwt.fail Vm.Node_not_found
                  | Some(t) ->
                      match t with
                      | Dir(_) -> Lwt.return t
                      | _ -> Lwt.fail Vm.Wrong_node_kind
        end


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


let rec get_nb_cancel_char cancel_char nb_cur_char res l = match l with
  | [] -> res
  | h::t ->
      if ((nb_cur_char == 0) || (String.length h == 0)) then res
      else if (h.[0] == cancel_char) then
	  get_nb_cancel_char cancel_char (nb_cur_char-1) (res+1) t
      else res


(** Parse le résultat de darcs diff pour stocker son contenu dans
    une liste de types file_diff *)
let rec parse_diff diff_res parsed_res started = match diff_res with
  | [] ->
      let currentDiff = List.hd parsed_res in
      Lwt.return {fileName=currentDiff.fileName;
		  oldContent = List.rev(currentDiff.oldContent);
		  newContent = List.rev(currentDiff.newContent)}
  | h::i::t ->
      let currentDiff = List.hd parsed_res in
      if
	(String.length h > 2 &&
	 ((h.[0] == '-' && h.[1] == '-' && h.[2] == '-')
       || (h.[0] == '+' && h.[1] == '+' && h.[2] == '+'))) then
	parse_diff (i::t) parsed_res true
      else if (started == false) then
	parse_diff (i::t) parsed_res started
      else begin
	if (String.length h == 0) then parse_diff (i::t) parsed_res started
	else if (h.[0] == '+') then
	  if (String.length i > 0) && (i.[0] == '-') then
	    let new_res = {fileName = currentDiff.fileName;
			   oldContent = ((Diff,(string_after i 1))::
					 currentDiff.oldContent);
			   newContent = ((Diff,(string_after h 1))::
					 currentDiff.newContent)}::
	      (List.tl parsed_res)
	    in parse_diff t new_res started
	  else if (String.length i > 0) then
	    let new_res = {fileName = currentDiff.fileName;
			   oldContent =
			   ((Blank,(String.make (String.length h -1) ' '))::
			    currentDiff.oldContent);
			   newContent = ((Diff,(string_after h 1))::
					 currentDiff.newContent)}::
	      (List.tl parsed_res)
	    in
	    parse_diff (i::t) new_res started
	  else
	    let new_res = {fileName = currentDiff.fileName;
			   oldContent = currentDiff.oldContent;
			   newContent = ((Diff,(string_after h 1))::
					 currentDiff.newContent)}::
	      (List.tl parsed_res)
	    in
	    parse_diff t new_res started
	else if (h.[0] == '-') then
	  if (String.length i > 0) && (i.[0] == '+') then
	    let new_res = {fileName = currentDiff.fileName;
			   oldContent = ((Diff,(string_after h 1))::
					 currentDiff.oldContent);
			   newContent = ((Diff,(string_after i 1))::
					 currentDiff.newContent)}::
	      (List.tl parsed_res)
	    in parse_diff t new_res started
	  else if (String.length i > 0) then
	    let new_res = ({fileName = currentDiff.fileName;
			    oldContent =
			    ((Diff,(string_after h 1))::currentDiff.oldContent);
			    newContent =
			    (Blank,(String.make (String.length h -1) ' '))::currentDiff.newContent})::
	      (List.tl parsed_res)
	    in
	    parse_diff (i::t) new_res started
	  else
	    let new_res = ({fileName = currentDiff.fileName;
			    oldContent =
			    ((Diff,(string_after h 1))::currentDiff.oldContent);
			    newContent = currentDiff.newContent})::
	      (List.tl parsed_res)
	    in
	    parse_diff t new_res started
	else if (h.[0] != '@') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = ((Common,(string_after h 1))::
				       currentDiff.oldContent);
			 newContent = ((Common,(string_after h 1))::
				       currentDiff.newContent)}
	    ::(List.tl parsed_res)
	  in
	  parse_diff (i::t) new_res started
	else parse_diff (i::t) parsed_res started
      end
  | [h] ->
      let t = [] in
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
			 oldContent = (Blank,(String.make (String.length h -1) ' '))::currentDiff.oldContent;
			 newContent = ((Diff,(string_after h 1))::
				       currentDiff.newContent)}::
	    (List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] == '-') then
	  let new_res = ({fileName = currentDiff.fileName;
			  oldContent =
			  ((Diff,(string_after h 1))::currentDiff.oldContent);
			  newContent = (Blank,(String.make (String.length h -1) ' '))::currentDiff.newContent})::
	    (List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] != '@') then
	  let new_res = {fileName = currentDiff.fileName;
			 oldContent = ((Common,(string_after h 1))::
				       currentDiff.oldContent);
			 newContent = ((Common,(string_after h 1))::
				       currentDiff.newContent)}
	    ::(List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else parse_diff t parsed_res started
      end


(** Execute la commande darcs diff et applique parse_diff sur le résultat *)
let darcs_diff file rep from_patch to_patch =
  let error_message = "Error while getting diff result (signal received)" in
  let command = ("darcs diff --from-match 'hash "^from_patch^
		 "' --to-match 'hash "^to_patch^
		 "' "^file^
		 " -u --repodir "^rep) in
  exec_command command error_message >>= fun res_string ->
    let res = split (regexp "\n") res_string in
    parse_diff res [{fileName=file; oldContent=[]; newContent=[]}] false


(** Execute la commande darcs diff sur 2 patches *)
let darcs_patchdiff rep from_patch to_patch =
  let error_message = "Error while getting patchdiff result (signal received)"
  in
  let command = ("darcs diff --from-match 'hash "^from_patch^
		 "' --to-match 'hash "^to_patch^
		 "' -u --repodir "^rep) in
  exec_command command error_message



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

let darcs_annot ?id rep file =
  let error_message = "Error while annotating the file (signal received)" in
  let command = match id with
    | None -> ("darcs annotate --xml-output "^file^" --repodir="^rep)
    | Some(patch) -> ("darcs annotate --xml-output --match 'hash "^patch^
                      "' "^file^" --repodir="^rep)
  in
  exec_command command error_message >>= fun s ->
    let res = Simplexmlparser.xmlparser_string s in
    handle_annot_xml_list [] res >>= fun l -> Lwt.return (List.rev l)



let _ =
  let darcs_fun_pack =
    { vm_list = darcs_list;
      vm_cat = darcs_cat;
      vm_log = darcs_log;
      vm_diff = darcs_diff;
      vm_patchdiff = darcs_patchdiff;
      vm_annot = darcs_annot} in
  Ocsforge_version_managers.set_fun_pack "Darcs" darcs_fun_pack

