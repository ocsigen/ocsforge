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

open Ocsforge_source_types


(** Supprime un élément d'une liste *)
let rec remove_node node l = match l with
  | [] -> raise Not_found
  | h::t ->
      begin match (h,node) with
      | (Dir(n1,_),Dir(n2,_))
      | (File(n1,_,_),File(n2,_,_)) ->
	  if((String.compare n1 n2) == 0) then
            t
	  else
	    h::(remove_node node t)
      | _ -> h::(remove_node node t)
      end


(** Renvoie l'élément de nom recherché *)
let rec find_node name l = match l with
  | [] -> raise Not_found
  | h::t ->
      begin match h with
        | Dir(n,_)
	| File(n,_,_) ->
	    if((String.compare n name) == 0) then
	      h
	    else
	      find_node name t
      end


(** Insere un élément dans l'arbre *)
let rec insert node target tree = match (target,tree) with
  | ([],Dir(n,l)) ->
      let name = match node with
      | File(f,_,_) -> f
      | Dir(d,_) -> d
      in
      begin try
	ignore(find_node name l);
        (*failwith ("Tentative d'insérer un noeud déja présent : "^name);*)
	Dir(n,l)
      with Not_found ->
	Dir(n,node::l)
      end
  | (_,Dir(n,l)) ->
       let rec insert_aux node target content = match content with
         | [] ->
	     let path = (List.hd target) in
	     [insert node (List.tl target) (Dir(path,[]))]
         | Dir(n,l)::t ->
	     if ((String.compare n (List.hd target)) == 0) then
		 (insert node (List.tl target) (Dir(n,l)))::t
	     else
	       (Dir(n,l)::(insert_aux node target t))
	 | h::t ->
	     h::(insert_aux node target t)
       in
       Dir(n,insert_aux node target l)
  | (_,_) -> failwith "File tree insertion error : not a directory."


(** Supprime un élément de l'arbre *)
let rec delete node target tree = match (target,tree) with
  |([],Dir(n,l)) ->
      Dir(n,(remove_node node l))
  | (_,Dir(n,l)) ->
       let rec delete_aux node target content = match content with
         | [] -> []

             (*let path = (List.hd target) in
             print_endline ("File tree deletion error: directory "^path^" does not exist.");
             []*)
	     (*
	     failwith ("File tree deletion error: directory "^path^" does not exist.")*)
         | Dir(n,l)::t ->
	     if ((String.compare n (List.hd target)) == 0) then
	       (delete node (List.tl target) (Dir(n,l)))::t
	     else
	      (Dir(n,l)::(delete_aux node target t))
	 | File(f,a,r)::t ->
	     File(f,a,r)::(delete_aux node target t)
       in
       Dir(n,delete_aux node target l)
  | (_,_) -> failwith "File tree deletion error : not a directory."


(** Recupere un noeud dans l'arbre *)
let rec get_node name target tree = match (target,tree) with
  |([],Dir(_,l)) ->
      begin
	try
	  Some(find_node name l)
	with Not_found ->
	  None
      end
  | (_,Dir(_,l)) ->
      let rec get_aux name target content = match content with
      | [] -> None
      | Dir(n,l)::t ->
	  if ((String.compare n (List.hd target)) == 0) then
	    get_node name (List.tl target) (Dir(n,l))
	  else
	    get_aux name target t
      | File(_,_,_)::t -> get_aux name target t
      in
      get_aux name target l
  | (_,_) -> failwith "File tree get node error : not a directory."


let rec rename_node path old_name new_name tree = match (path,tree) with
  | ([],Dir(n,l)) -> (* on est dans le bon répertoire *)
      let rec rename l = match l with
      | [] -> []
      | File(n,a,i)::t ->
          if (String.compare n old_name == 0) then
            (File(new_name,a,i)::t)
          else (File(n,a,i)::(rename t))
      | Dir(n',l')::t ->
          if (String.compare n' old_name == 0) then (Dir(new_name,l')::t)
          else (Dir(n',l')::(rename t))
      in
      Dir(n,(rename l))
  | (_, Dir(n,l)) -> (* on cherche le bon répertoire *)
      let d = Dir(n,
                  (List.map
                     (fun entry -> match entry with
                     | Dir(n',_) ->
                         if (n' = List.hd path) then
                           rename_node (List.tl path) old_name new_name entry
                         else entry
                     | _ -> entry) l)) in
      (*if (d = Dir(n,l)) then
        print_endline ("[RATE] : "^(String.concat "/" path)^" "^old_name^"->"^new_name)
      else print_endline ("[OK] : "^(String.concat "/" path)^" "^old_name^"->"^new_name);*)
      d
  | _ -> (* on est sur un fichier : on a pas trouvé le bon répertoire *)
      tree


(** Deplace un élément de l'arbre *)
let move oldPath oldName newPath newName tree =
  (*if (oldPath = newPath) then
    rename_node oldPath oldName newName tree
  else*)
    let node_opt = get_node oldName oldPath tree in match node_opt with
    | None ->
        (*failwith ("Move error: node "^(String.concat "/" oldPath)^"/"^oldName^"not found")*)
	tree
    | Some(node) ->
	let tmp = delete node oldPath tree in
	let newNode = match node with
	| File(_,a,r) -> File(newName,a,r)
	| Dir(_,l) -> Dir(newName,l)
	in
	insert newNode newPath tmp

(** Changes l'auteur et la derniere version d'un noeud de l'arbre *)
let update_infos path name newAut newVersName newVersID tree =
  let node = get_node name path tree in
  match node with
    | Some(File(f,a,b)) ->
	let tmp = delete (File(f,a,b)) path tree in
	let newNode = File(f,newAut,(newVersName,newVersID)) in
	insert newNode path tmp
    | Some(Dir(_,_))
    | None-> tree
