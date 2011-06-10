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

open Lwt
(* Type représentant l'arborescence du répertoire *)
(* rep_tree =
   File(Nom Fichier, Nom auteur, (derniere version Nom * derniere version ID))
   | Dir(Nom repertoire, rep_tree list) *)
type rep_tree =
  | File of (string * string * (string * string))
  | Dir of (string * rep_tree list)

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
type cat_result = string Lwt.t

(* Type permettant de différencier les lignes
   identiques/différentes lors d'un diff *)
type rowType = Common | Diff | Blank

(* Type représentant les différences entre 2 versions d'un fichier *)
type file_diff = {fileName: string;
		  oldContent: (rowType*string) list;
		  newContent: (rowType*string) list}

(* Type représentant le résultat d'un diff *)
type diff_result = file_diff Lwt.t

(* Type représentant le résultat d'un diff entre 2 patchs *)
(* Le résultat doit être transmis non traité *)
type patchdiff_result = string Lwt.t

(* Type représentant le résultat d'un annotate *)
(* (Nom auteur * ligne) *)
type annot_result = (string * string) list Lwt.t

(* Type représentant l'ensemble des fonctions dont ont besoin les pages du site*)
type fun_pack = {vm_list: (?id:string -> ?dir:string -> string -> list_result);
		 vm_cat: (?id:string -> string -> string -> cat_result);
		 vm_log: (?file:string -> ?range:(string option * string option) -> ?limit:int -> string -> log_result);
		 vm_diff: (string -> string -> string -> string -> diff_result);
                 vm_patchdiff: (string -> string -> string -> patchdiff_result);
                 vm_annot: (?id:string -> string -> string -> annot_result)}

