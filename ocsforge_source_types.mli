open Lwt
(* Type représentant l'arborescence du répertoire *)
(* rep_tree = 
   File(Nom Fichier, Nom auteur, (derniere version Nom * derniere version ID)) 
   | Dir(Nom repertoire, rep_tree list) *)
type rep_tree = 
  | File of ((string ref) * string * (string * string)) 
  | Dir of ((string ref) * rep_tree list) 

(* type représentant un patch sous gestionnaire de version *)
type patch = {id: string ref;
              name: string ref;
	      author: string ref; 
	      date: string ref;
	      comment: string ref;
	      tree: rep_tree}

(* Type représentant le résultat d'un ls *)
type list_result = rep_tree Lwt.t

(* Type représentant le résultat d'un log *)
type log_result = patch list Lwt.t

(* Type représentant le résultat d'un cat *)
type cat_result = string list Lwt.t

(* Type permettant de différencier les lignes 
   identiques/différentes lors d'un diff *)
type rowType = Common | Diff | Blank

(* Type représentant les différences entre 2 versions d'un fichier *)
type file_diff = {fileName: string; 
		  oldContent: (rowType*string) list;
		  newContent: (rowType*string) list}

(* Type représentant le résultat d'un diff *)
type diff_result = file_diff Lwt.t

(* Type représentant l'ensemble des fonctions dont ont besoin les pages du site*)
type fun_pack = {vm_list: (?id:string -> string -> list_result);
		 vm_cat: (?id:string -> string -> string -> cat_result);
		 vm_log: (?file:string -> string -> log_result);
		 vm_diff: (string -> string -> string -> string -> diff_result)}
  
