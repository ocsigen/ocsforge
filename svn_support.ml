open Netstring_pcre
open Ocsforge_source_types
open Lwt

(** Compilation :

% swig -ocaml swig_svn.i
% swig -ocaml -co swig.mli ; swig -ocaml co swig.ml
% ocamlc -c swig.mli ; ocamlc -c swig.ml
% ocamlc -c -ccopt "-I/usr/include/subversion-1 -I/usr/include/apr-1.0 -D_LARGEFILE64_SOURCE" swig_svn_wrap.c
% ocamlc -c -ccopt "-I/usr/include/subversion-1 -I/usr/include/apr-1.0 -D_LARGEFILE64_SOURCE" swig_svn.c
% ocamlc -c swig_svn.mli
% ocamlc -c swig_svn.ml
% ocamlc -o test swig.cmo swig_svn.cmo -custom swig_svn_wrap.o swig_svn.o -cclib -lsvn_client-1 svn_support.ml

*)

module Tree = Ocsforge_source_tree

let index_regexp = regexp "Index: "

(** Extrait la liste Caml depuis une C_list *)
let extract_list res = match res with
  | Swig.C_list(l) -> l
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
	      create_tree t (Dir(n,File(ref(List.hd ename),author,(rev,rev))::l)) name author next_step
	    else begin
	      let target =  build_target ename in
	      if (name.[(String.length name - 1)] == '/') then begin 
		let node_name = List.nth ename (length - 2) in
		let new_tree = (Tree.insert (Dir(ref(node_name),[])) target tree) in
		create_tree t new_tree name author next_step
	      end
	      else begin
		let node_name = List.nth ename (length - 1) in 
		let new_tree = (Tree.insert (File(ref(node_name),author,(rev,rev))) target tree) in
		create_tree t new_tree name author next_step
	      end
	    end
	| _ -> failwith "File tree insertion error : not a directory."
	end
      
      

(** Stocke la liste des repertoires/fichiers sous gestionnaire de version 
    à la révision demandée dans un arbre de type rep_tree (si aucune révision
    n'est précisée, renvoie la version la plus récente) *)
let svn_list ?id ?dir rep = 
  let svn_result = match (id,dir) with
  | (None,None) ->
      extract_list (Swig_svn._svn_support_list (Swig.C_list[
				       Swig.C_string(rep);
      				       Swig.C_int(-1)]))
  | (Some(s),None) ->
      let r = int_of_string s in
      extract_list (Swig_svn._svn_support_list (Swig.C_list[
				       Swig.C_string(rep);
				       Swig.C_int(r)]))
  | (None,Some(d)) ->
      extract_list (Swig_svn._svn_support_list (Swig.C_list[
				                Swig.C_string(rep^"/"^d);
      				                Swig.C_int(-1)]))
  | (Some(s),Some(d)) ->
      let r = int_of_string s in
      extract_list (Swig_svn._svn_support_list (Swig.C_list[
				                Swig.C_string(rep^"/"^d);
				                Swig.C_int(r)]))

  in
  let list_res = 
    List.map (fun s -> 
      match s with
      | Swig.C_string(s) -> s
      | _ -> "") (svn_result) 
  in
  create_tree list_res  (Dir(ref(""),[])) "" "" 0
       
(** Crée la liste des patchs à partir du resultat de svn_log  *)
let rec create_patch_list log_res patch_list step = match log_res with
  | [] -> Lwt.return patch_list
  | h::t -> 
      let next_step = step+1 in
      if (step mod 5 == 0) then begin
	let next_patch = {id = ref h; name= ref h; author = ref "";
			  date = ref ""; comment = ref ""; tree = File(ref(""),"",("",""))} 
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
let svn_log ?file ?id ?limit rep = 
  let last = match limit with
    | None -> 0
    | Some(i) -> i
  in
  let log_res = 
    match file with
      | None    -> 
	  List.map 
	    (fun s -> 
	      match s with
	        | Swig.C_string(s) -> s
		| _ -> "")
	    (extract_list (Swig_svn._svn_support_log 
			     (Swig.C_list
				[Swig.C_string(rep); Swig.C_int(last)])))
      | Some(f) ->
	  List.map 
	    (fun s -> match s with
	                | Swig.C_string(s) -> s
			| _ -> "")
	    (extract_list (Swig_svn._svn_support_log 
			     (Swig.C_list
				[Swig.C_string(rep^"/"^f); Swig.C_int(last)])))
  in
  create_patch_list log_res [] 0

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
let svn_diff file rep id1 id2 =
  let rev1 = int_of_string id1 in
  let rev2 = int_of_string id2 in 
  let diff_res =  
    List.map (fun s -> 
      match s with
      | Swig.C_string(s) -> s
      | _ -> "") (extract_list (Swig_svn._svn_support_diff(Swig.C_list[Swig.C_string(rep); 
							 Swig.C_string(file);
							 Swig.C_int(rev1);
							 Swig.C_int(rev2)])))
  in
  parse_diff diff_res [{fileName = file; oldContent = []; newContent = []}] false

(** Récupère le contenu d'un fichier à la révision demandée (si aucune
    révision n'est précisée, renvoie la version la plus récente) *)
let svn_cat ?id rep file =
  let file_path = (rep^"/"^file) in
  let cat_res = match id with
  | None ->
      extract_list (Swig_svn._svn_support_cat(
		    Swig.C_list[Swig.C_string(file_path);Swig.C_int(-1)]))
  | Some(s) -> 
      let r = int_of_string s in
      extract_list (Swig_svn._svn_support_cat(
		    Swig.C_list[Swig.C_string(file_path);Swig.C_int(r)]))
  in
  Lwt_util.map(fun s ->
    match s with
    | Swig.C_string(s) -> Lwt.return s
    | _ -> Lwt.return "") (cat_res) 


let rec extract_annot_result step res l = match l with
  | [] -> Lwt.return (List.rev res)
  | h::t ->
      if (step mod 2 == 0) then (* author *)
        extract_annot_result (step+1) ((h,"")::res) t
      else (* line *)
        let aut = fst (List.hd res) in
        extract_annot_result (step+1) ((aut,h)::(List.tl res)) t


let svn_annot ?id rep file = 
  let file_path = (rep^"/"^file) in
  let annot_res = match id with
  | None ->
      extract_list (Swig_svn._svn_support_blame(
                    Swig.C_list[Swig.C_string(file_path);Swig.C_int(-1)]))
  | Some(s) ->
      let r = int_of_string s in
      extract_list (Swig_svn._svn_support_blame(
		    Swig.C_list[Swig.C_string(file_path);Swig.C_int(r)]))
  in
  Lwt_util.map 
    (fun s -> match s with
    | Swig.C_string(s) -> Lwt.return s
    | _ -> Lwt.return "")
    (annot_res) >>= fun l ->
      extract_annot_result 0 [] l

let _ = 
  (** test svn list *)
  (*let res = svn_list "https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge"*)
  (** test svn log *)
  (*let res = svn_log "https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge"*)
 
  (** test svn cat *)
  (*let res = svn_cat ~rev:27 "https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge" "README" *)
  
  (** test svn diff *) 
  (*
  svn_diff "pages.ml" "https://forge.mancoosi.univ-paris-diderot.fr/svn/ocsforge" "125" "140" >>= fun res ->
    List.iter (fun (_,p) -> print_endline p) res.oldContent;
    print_endline "********************************";
    List.iter (fun (_,p) -> print_endline p) res.newContent;
    Lwt.return (print_endline "FIN")
      
	*)
	let svn_fun_pack = 
	{vm_list = svn_list;
	 vm_cat = svn_cat;
	 vm_log = svn_log;
	 vm_diff = svn_diff;
         vm_annot = svn_annot} in
	Ocsforge_version_managers.set_fun_pack "SVN" svn_fun_pack
      
