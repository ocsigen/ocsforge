(* Ocsimore
 * Copyright (C) 2005
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

open Netstring_pcre
open Ocsforge_source_types
open Lwt

module Tree = Ocsforge_source_tree
module Vm = Ocsforge_version_managers

let index_regexp = regexp "Index: "

let svn_repo_format rep =
  let l = split (regexp "/") rep in
  Lwt.return (String.concat "/" l) (*
  try
    if (rep.[String.length rep - 1] = '/') then
      Lwt.return (String.sub rep 0 (String.length rep - 1))
    else Lwt.return rep
  with _ ->
    Lwt.fail Vm.Manager_command_error*)

(** Extrait la liste Caml depuis une C_list *)
let extract_list res = match res with
  | Swig.C_list(l) -> l
  | Swig.C_string(_) -> [res]
  | _ -> []


let build_target path_list =
  if (String.compare (List.nth path_list 1) "" == 0) then []
  else List.rev (List.tl (List.rev path_list))


(** Cree l'arboresence à partir du résultat de "svn list" *)
let rec create_tree list_res tree name author step = match list_res with
  | [] -> Lwt.return tree
  | h::t ->
      let next_step = step+1 in
      if (step mod 3 == 0) then begin
	create_tree t tree h author next_step
      end
      else if (step mod 3 == 1) then
	create_tree t tree name h next_step
      else
	let rev = h in
	let ename = Netstring_pcre.split_delim (regexp "/") name in
        begin match tree with
	| Dir(n,l) ->
	    let length = List.length ename in
	    if (length == 1) then
	      create_tree t (Dir(n,File(List.hd ename,author,(rev,rev))::l)) name author next_step
	    else begin
	      let target =  build_target ename in
	      if (name.[(String.length name - 1)] == '/') then begin
		let node_name = List.nth ename (length - 2) in
		let new_tree = (Tree.insert (Dir(node_name,[])) target tree) in
		create_tree t new_tree name author next_step
	      end
	      else begin
		let node_name = List.nth ename (length - 1) in
		let new_tree = (Tree.insert (File(node_name,author,(rev,rev))) target tree) in
		create_tree t new_tree name author next_step
	      end
	    end
	| _ -> failwith "File tree insertion error : not a directory."
	end



(** Stocke la liste des repertoires/fichiers sous gestionnaire de version
    à la révision demandée dans un arbre de type rep_tree (si aucune révision
    n'est précisée, renvoie la version la plus récente) *)
let svn_list ?id ?dir repository =
  svn_repo_format repository >>= fun rep ->
  let list_call () = match (id,dir) with
    | (None,None) ->
        Swig_svn._svn_support_list (Swig.C_list[
				    Swig.C_string(rep);
      				    Swig.C_int(-1)])
    | (Some(s),None) ->
        let r = int_of_string s in
        Swig_svn._svn_support_list (Swig.C_list[
				    Swig.C_string(rep);
				    Swig.C_int(r)])
    | (None,Some(d)) ->
        Swig_svn._svn_support_list (Swig.C_list[
				    Swig.C_string(rep^"/"^d);
      				    Swig.C_int(-1)])
    | (Some(s),Some(d)) ->
        let r = int_of_string s in
        Swig_svn._svn_support_list (Swig.C_list[
				    Swig.C_string(rep^"/"^d);
				    Swig.C_int(r)])

    in
    Lwt.catch
    (fun () ->
      Lwt_preemptive.detach list_call () >>= fun l ->
      let svn_result = (extract_list l) in
      let list_res =
        List.map (fun s ->
          match s with
          | Swig.C_string(s) -> s
          | _ -> "") (svn_result)
      in
      create_tree list_res  (Dir("",[])) "" "" 0)
    (function
      | Failure _ ->
          Lwt.fail Vm.Wrong_node_kind
      | e -> Lwt.fail e)


(** Crée la liste des patchs à partir du resultat de svn_log  *)
let rec create_patch_list log_res patch_list step = match log_res with
  | [] -> Lwt.return patch_list
  | h::t ->
      let next_step = step+1 in
      if (step mod 5 == 0) then begin
	let next_patch = {id = ref h; name= ref h; author = ref "";
			  date = ref ""; comment = ref ""; tree = File("","",("",""))}
	in
	create_patch_list t (next_patch::patch_list) next_step
      end
      else begin
	if (step mod 5 == 1) then
	  (List.hd patch_list).author := h
	else if (step mod 5 == 2) then
	  (List.hd patch_list).date := h
	else if (step mod 5 == 3) then
	  (List.hd patch_list).date := ((!((List.hd patch_list).date))^" "^h)
	else begin
	  let comment = global_replace (regexp "<br/>") "\n" h
	  in (List.hd patch_list).comment := comment
	end;
	create_patch_list t patch_list next_step
      end


(** Stocke la liste des révisions du dépôt dans une liste de type patch *)
let svn_log ?file ?range ?limit repository =
  svn_repo_format repository >>= fun rep ->
  let last = match limit with
    | None -> 0
    | Some(i) -> i
  in
  let (start_rev,end_rev) = match range with
    | None | Some(None,None) -> (-1,-1)
    | Some(None,Some(er)) ->
        (-1, int_of_string er)
    | Some(Some(sr),None) ->
        (int_of_string sr, -1)
    | Some(Some(sr),Some(er)) -> (int_of_string sr,int_of_string er)
  in
  let path = match file with
    | None -> rep
    | Some(f) -> (rep^"/"^f)
  in
  try
    let a () =
      if (start_rev != -1 && end_rev != -1) then
        (List.map
	   (fun s ->
	     match s with
	     | Swig.C_string(s) -> s
	     | _ -> "")
	   (extract_list (Swig_svn._svn_support_log
			    (Swig.C_list
			       [Swig.C_string(path);
                                Swig.C_int(end_rev);
                                Swig.C_int(-1);
                                Swig.C_int(2*last)]))),
         List.map
	   (fun s ->
	     match s with
	     | Swig.C_string(s) -> s
	     | _ -> "")
           (extract_list (Swig_svn._svn_support_log
                            (Swig.C_list
		               [Swig.C_string(path);
                                Swig.C_int(-1);
                                Swig.C_int(end_rev);
                                Swig.C_int(last)]))))
      else
        ([],
         List.map
	   (fun s ->
	     match s with
	     | Swig.C_string(s) -> s
	     | _ -> "")
	   (extract_list (Swig_svn._svn_support_log
			    (Swig.C_list
			       [Swig.C_string(path);
                                Swig.C_int(start_rev);
                                Swig.C_int(end_rev);
                                Swig.C_int(3*last)]))))
    in
    Lwt_preemptive.detach a () >>= fun (l1,l2) ->
      create_patch_list l1 [] 0 >>= fun tmp ->
        let tl_rev_tmp = match tmp with
        | [] -> []
        | _ -> List.tl (List.rev tmp)
        in
        create_patch_list l2 tl_rev_tmp 0 >>= fun l ->
          Lwt.return (List.rev l)
  with _ -> Lwt.fail Vm.Manager_command_error

let rec tabcount s i =
  if i < String.length s then
    if s.[i] == '\t' then (1 + (tabcount s (i+1)))
    else tabcount s (i+1)
  else 0

(** Parse le résultat de svn_diff pour stocker son contenu dans
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
			   ((Blank,(String.make (String.length h + 7*(tabcount h 0) - 1) ' '))::
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
			    (Blank,(String.make (String.length h + 7*(tabcount h 0) - 1) ' '))::currentDiff.newContent})::
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
			 oldContent = (Blank,(String.make
                                                (String.length h +
                                                   7*(tabcount h 0) - 1) ' '))
                         ::currentDiff.oldContent;
			 newContent = ((Diff,(string_after h 1))::
				       currentDiff.newContent)}::
	    (List.tl parsed_res)
	  in
	  parse_diff t new_res started
	else if (h.[0] == '-') then
	  let new_res = ({fileName = currentDiff.fileName;
			  oldContent =
			  ((Diff,(string_after h 1))::currentDiff.oldContent);
			  newContent = (Blank,(String.make
                                                 (String.length h +
                                                    7*(tabcount h 0) - 1) ' '))
                          ::currentDiff.newContent})::
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


(** Stocke le résultat du diff sur file@rev1 / file@rev2
    dans une liste de type file_diff *)
let svn_diff file repository id1 id2 =
  svn_repo_format repository >>= fun rep ->
  try
    let rev1 = int_of_string id1 in
    let rev2 = int_of_string id2 in
    let diff_call () =
      Swig_svn._svn_support_diff(Swig.C_list[Swig.C_string(rep);
					     Swig.C_string(file);
					     Swig.C_int(rev1);
					     Swig.C_int(rev2)])
    in
    Lwt_preemptive.detach diff_call () >>= fun l ->
      let diff_res = extract_list l in
      let map =
        List.map (fun s ->
          match s with
          | Swig.C_string(s) -> s
          | _ -> "") diff_res
      in
      parse_diff map [{fileName = file; oldContent = []; newContent = []}] false
  with _ -> Lwt.fail Vm.Manager_command_error

(** Stocke le résultat du diff entre id1 et id2 dans une chaine *)
let svn_patchdiff repository id1 id2 =
  svn_repo_format repository >>= fun rep ->
  try
    let rev1 = int_of_string id1 in
    let rev2 = int_of_string id2 in
    let diff_call () =
      Swig_svn._svn_support_diff(Swig.C_list[Swig.C_string(rep);
					     Swig.C_string("");
					     Swig.C_int(rev1);
					     Swig.C_int(rev2)])
    in
    Lwt_preemptive.detach diff_call () >>= fun l ->
      let diff_res = extract_list l in
      let map =
        List.map (fun s ->
          match s with
          | Swig.C_string(s) -> s
          | _ -> "") diff_res
      in
      Lwt.return (String.concat "\n" map)
  with _ -> Lwt.fail Vm.Manager_command_error

(** Récupère le contenu d'un fichier à la révision demandée (si aucune
    révision n'est précisée, renvoie la version la plus récente) *)
let svn_cat ?id repository file =
  svn_repo_format repository >>= fun rep ->
  try
    let file_path = (rep^"/"^file) in
    let cat_call () =
      match id with
      | None ->
          Swig_svn._svn_support_cat(
	  Swig.C_list[Swig.C_string(file_path);Swig.C_int(-1)])
      | Some(s) ->
          let r = int_of_string s in
          Swig_svn._svn_support_cat(
	  Swig.C_list[Swig.C_string(file_path);Swig.C_int(r)])
    in
    Lwt_preemptive.detach cat_call () >>= fun l ->
      let cat_res = extract_list l in
      let map = List.map (fun s ->
        match s with
        | Swig.C_string(s) -> s
        | _ -> "") cat_res
      in
      Lwt.return (String.concat "\n" map)
  with _ -> Lwt.fail Vm.Manager_command_error


let rec extract_annot_result step res l = match l with
  | [] -> Lwt.return (List.rev res)
  | h::t ->
      if (step mod 2 == 0) then (* author *)
        extract_annot_result (step+1) ((h,"")::res) t
      else (* line *)
        let aut = fst (List.hd res) in
        extract_annot_result (step+1) ((aut,h)::(List.tl res)) t


let svn_annot ?id repository file =
  svn_repo_format repository >>= fun rep ->
  try
    let file_path = (rep^"/"^file) in
    let annot_call () = match id with
    | None ->
        Swig_svn._svn_support_blame(
        Swig.C_list[Swig.C_string(file_path);Swig.C_int(-1)])
    | Some(s) ->
        let r = int_of_string s in
        Swig_svn._svn_support_blame(
	Swig.C_list[Swig.C_string(file_path);Swig.C_int(r)])
    in
    Lwt_preemptive.detach annot_call () >>= fun l ->
      let annot_res = extract_list l in
      let map = List.map
          (fun s -> match s with
          | Swig.C_string(s) -> s
          | _ -> "")
          (annot_res)
      in
      extract_annot_result 0 [] map
  with _ -> Lwt.fail Vm.Manager_command_error

let _ =
  ignore(Swig_svn._svn_init (Swig.C_void));
  let svn_fun_pack =
    {vm_list = svn_list;
     vm_cat = svn_cat;
     vm_log = svn_log;
     vm_diff = svn_diff;
     vm_patchdiff = svn_patchdiff;
     vm_annot = svn_annot} in
  Ocsforge_version_managers.set_fun_pack "SVN" svn_fun_pack

