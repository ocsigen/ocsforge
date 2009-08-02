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

type src_page_kind = [
  | `Browse
  | `Cat
  | `Annot
  | `Diff
  | `PatchDiff
  | `Options 
  | `Error ]

type log_page_kind = [ `Log of (string option * string option) ]

(* Used by the menu *)
type page_kind = [ src_page_kind | log_page_kind ]
 

let kind_to_string kind = match kind with
  | `Browse -> "browse"
  | `Diff -> "diff"
  | `PatchDiff -> "patchdiff"
  | `Cat -> "content"
  | `Annot -> "annot"
  | `Options -> "options"
  | `Error -> "error"
      
let string_to_kind s = 
  if (s = "browse") then `Browse
  else if (s = "diff") then `Diff
  else if (s = "patchdiff") then `PatchDiff
  else if (s = "content") then `Cat
  else if (s = "annot") then `Annot
  else if (s = "options") then `Options
  else `Error


type project_services = 
    { sources_service:
	(string list * (src_page_kind option * (string option * string option))
           , unit,
         [ `Attached of
           Eliom_services.get_attached_service_kind Eliom_services.a_s ],
         [ `WithSuffix ],
         ([`One of string list] Eliom_parameters.param_name *
	    ([ `One of src_page_kind ] Eliom_parameters.param_name *
               ([ `One of string ] Eliom_parameters.param_name *
                  [ `One of string ] Eliom_parameters.param_name)))
            ,unit,
         [ `Registrable ])
	Eliom_services.service;
      log_service:
	((string option * string option) option, unit,
	 [ `Attached of
	   Eliom_services.get_attached_service_kind Eliom_services.a_s ],
	 [ `WithoutSuffix ],
	 ([ `One of (string option * string option)] Eliom_parameters.param_name), 
          unit,[ `Registrable ])
	Eliom_services.service
    }


(* Hashtable (task id, service) *)
let repos_services_table : (string,project_services) Hashtbl.t = 
  Hashtbl.create 50

let add_service id service = 
  Hashtbl.add repos_services_table id service

let find_service id = 
  try 
    let service = Hashtbl.find repos_services_table id 
    in Some(service)
  with Not_found ->
    None
