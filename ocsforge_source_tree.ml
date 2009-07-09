open Ocsforge_source_types


(** Supprime un élément d'une liste *)
let rec remove_node node l = match l with
  | [] -> []
  | h::t -> 
      begin match (h,node) with
        | (Dir(n1,_),Dir(n2,_)) 
	| (File(n1,_,_),File(n2,_,_)) ->
	    if((String.compare !n1 !n2) == 0) then
	      remove_node node t
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
	    if((String.compare !n name) == 0) then
	      h
	    else
	      find_node name t
      end


(** Insere un élément dans l'arbre *)
let rec insert node target tree = match (target,tree) with
  | ([],Dir(n,l)) -> 
      let name = match node with
      | File(f,_,_) -> !f
      | Dir(d,_) -> !d
      in
      begin try 
	ignore(find_node name l);
	Dir(n,l)
      with Not_found ->
	Dir(n,node::l)
      end
  | (_,Dir(n,l)) -> 
       let rec insert_aux node target content = match content with
         | [] -> 
	     let path = (List.hd target) in
	     [insert node (List.tl target) (Dir(ref(path),[]))]
         | Dir(n,l)::t -> 
	     if ((String.compare !n (List.hd target)) == 0) then
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
	     failwith ("File tree deletion error: directory "^path^" does not exist.")*)     
         | Dir(n,l)::t -> 
	     if ((String.compare !n (List.hd target)) == 0) then
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
	  (*let path = (List.hd target) in
	  failwith ("File tree get node error: directory "^path^" does not exist.")*)     
      | Dir(n,l)::t -> 
	  if ((String.compare !n (List.hd target)) == 0) then
	    get_node name (List.tl target) (Dir(n,l))
	  else
	    get_aux name target t
      | File(_,_,_)::t -> get_aux name target t
      in
      get_aux name target l
  | (_,_) -> failwith "File tree get node error : not a directory."
	 

let rec rename_node path oldName newName tree = match (path,tree) with
   |([],Dir(_,l)) -> 
      let rec find_aux l = match l with
      | [] -> ()
      | (Dir(n,_))::t
      | (File(n,_,_))::t -> 
	  if (String.compare !n oldName == 0) then
	    n:=newName
	  else find_aux t
      in find_aux l
  | (_,Dir(_,l)) -> 
      let rec rename_aux name target content = match content with
      | [] -> ()
      | Dir(n,l)::t -> 
	  if ((String.compare !n (List.hd target)) == 0) then
	    rename_node (List.tl target) oldName newName (Dir(n,l))
	  else
	    rename_aux name target t
      | File(_,_,_)::t -> rename_aux name target t
      in
      rename_aux oldName path l
  | (_,_) -> ()
	 

  

(** Deplace un élément de l'arbre *)
let move oldPath oldName newPath newName tree =
  if (oldPath = newPath) then begin
    rename_node oldPath oldName newName tree;
    tree
  end
  else
    let node_opt = get_node oldName oldPath tree in match node_opt with
    | None -> 
	tree
    | Some(node) ->
	let tmp = delete node oldPath tree in
	let newNode = match node with
	| File(_,a,r) -> File(ref(newName),a,r)
	| Dir(_,l) -> Dir(ref(newName),l) 
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
