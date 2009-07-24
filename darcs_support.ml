open Lwt
open Netstring_pcre
open Ocsforge_source_types
open Ocsforge_source_tree
open Xmltypes

(** Expression régulière utilisée pour darcs diff *)
let index_regexp = regexp "Index: "

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
  let (outpipe,inpipe) = Lwt_unix.pipe_in () in
  let (err_outpipe,err_inpipe) = Lwt_unix.pipe_in () in
  let buf = String.create 4096 in
  let err_buf = String.create 4096 in
  let result = Buffer.create 4096 in
  let err_result = Buffer.create 4096 in
  let pid = Unix.fork () in
  if (pid == 0) then begin
    Lwt_unix.close outpipe;
    Lwt_unix.close err_outpipe;
    Unix.dup2 inpipe Unix.stdout;
    Unix.dup2 err_inpipe Unix.stderr;
    Unix.close inpipe;
    Unix.close err_inpipe;
    Unix.execv "/bin/sh" [| "/bin/sh"; "-c"; command |];
  end
  else begin
    Unix.close inpipe;
    Unix.close err_inpipe;
    read_input_channel buf result (Lwt_chan.in_channel_of_descr outpipe) >>= fun res ->
      read_input_channel err_buf err_result (Lwt_chan.in_channel_of_descr err_outpipe) >>= fun err ->
        Lwt_unix.waitpid [] pid >>= fun status -> match status with
          (*(0,_) -> wait_nohang ()*)
        | (_,Unix.WEXITED(0)) ->
            if (err = "") then
              Lwt.return res
            else begin 
              print_endline err;
              Lwt.fail Ocsforge_version_managers.Manager_command_error
            end
              (*begin prerr_endline err; Lwt.return res end*)
	| (_,_) -> 
	    Lwt.return error_message
  end


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
	      if (String.compare name "add_file" == 0) then
		handle_add_file res patch econtent
	      else if (String.compare name "remove_file" == 0) then
		handle_remove_file res patch econtent
	      else if (String.compare name "add_directory" == 0) then
		handle_add_directory res patch econtent
	      else if (String.compare name "remove_directory" == 0) then
		handle_remove_directory res patch econtent
	      else if (String.compare name "modify_file" == 0) then
		handle_modify_file res patch econtent
	      else if (String.compare name "move" == 0) then 
		handle_move res args
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
	  let parsed_author = split (regexp "&lt") v in
	  p.author := List.hd (parsed_author)
	else if (String.compare f "local_date" == 0) then
	  p.date := v
	else if (String.compare f "hash" == 0) then
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
	    if (String.compare name "patch" == 0) then
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
	    if (String.compare name "changelog" == 0) then begin
              handle_changelog res content >>= fun new_res ->
	      handle_log_xml_list new_res t
            end
	    else
	      handle_log_xml_list res t
	

let rec handle_annot_patch args = match args with
  | [] -> ""
  | (f,v)::t -> 
      if (String.compare f "author" == 0) then
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
          if (String.compare name "patch" == 0) then 
            handle_annot_patch args
          else
            handle_added_by t


let rec handle_normal_line aut line_content l = match l with
  | [] -> Lwt.return ((aut^"\n"),(line_content^"\n"))
  | h::t -> 
      match h with
        | Simplexmlparser.PCData(line) ->
            handle_normal_line aut (line_content^line) t
        | Simplexmlparser.Element(name,_,content) ->
            if (String.compare name "added_by" == 0) then
              handle_normal_line (handle_added_by content) line_content t
            else handle_normal_line aut line_content t
            

let rec handle_file res l = match l with
  | [] -> Lwt.return res
  | h::t ->
      match h with
        | Simplexmlparser.PCData(_) -> handle_file res t
        | Simplexmlparser.Element(name,_,content) -> 
            if (String.compare name "normal_line" == 0) then
              handle_normal_line "" "" content >>= fun line ->
                handle_file (line::res) t
            else
              handle_file res t

(** Recupere la liste (auteur,ligne) depuis le résultat darcs annotate *)      
let rec handle_annot_xml_list res l = match l with
  | [] -> Lwt.return res
  | h::t -> 
      match h with
        | Simplexmlparser.PCData(_) -> handle_annot_xml_list res t
        | Simplexmlparser.Element(name,_,content) -> 
            if (String.compare name "file" == 0) then
              handle_file [] content
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
  | (Some(None,Some(end_rev)),Some(l)) ->
      get_patch_index end_rev rep >>= fun num ->
          Lwt.return ("-n 1-"^(string_of_int (num+l)))
  | (Some(Some(start_rev),None),Some(l)) ->
      get_nb_patches rep >>= fun total ->
        get_patch_num start_rev rep >>= fun num ->
          let sr = max (total-num-l) 1 in
          Lwt.return ("-n "^(string_of_int sr)^"-"^(string_of_int total)) 
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
	   ^" "^index^"--xml-output --max-count "^(string_of_int (3*i))
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
              | None -> Lwt.return (Dir("",[])) (* a traiter ? *)
              | Some(t) -> Lwt.return t


(** Recuperes l'arbre associé au patch précisé *)
let darcs_list ?id ?dir repos_path = 
  let rec find_rev rev l = match l with
    | [] -> Lwt.return (Dir("",[])) (* a traiter ? *)
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
          | [] -> Lwt.return (Dir("",[])) (* a traiter ? *)
          | _ -> find_rev i l
        end
    | (None,Some(d)) -> 
        darcs_wc_list ~dir:d repos_path
    | (Some(i),Some(d)) -> 
        get_patch_list ~id:i ~dir:d repos_path >>= fun l -> begin match l with
          | [] -> Lwt.return (Dir("",[])) (* a traiter ? *)
          | _ -> 
              find_rev i l >>= fun tree -> 
                let (target,name) = format_name d ~eatblanks:false in
                let res = get_node name target tree in match res with
                  | None -> Lwt.return (Dir("",[])) (* a traiter ? *)
                  | Some(t) -> Lwt.return t
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
(*
let rec get_nb_char cur_char res l = match l with
  | [] -> 0
  | h::t ->
      if (String.length h > 0) && (h.[0] == cur_char) then
	get_nb_char cur_char (res+1) t
      else if (res > 0) then
	if (cur_char == '+') then
	  get_nb_cancel_char '-' res 0 t
	else
	  get_nb_cancel_char '+' res 0 t
*)
(*
let parse_diff_aux parsed_res nb_to_add nb_added cur_char l = 
  match l with
    | [] -> (parsed_res,[])
    | h::t -> 
	if  
*)

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
  exec_command command error_message >>= fun res_string ->
    let res = split (regexp "\n") res_string in
    parse_diff res [{fileName=file; oldContent=[]; newContent=[]}] false
	(*| [] -> Lwt.return {fileName=file; oldContent=[]; newContent=[]}
	| h::_ -> Lwt.return h *)


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
      vm_annot = darcs_annot} in
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

