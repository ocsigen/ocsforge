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

let (>>=) = Lwt.bind
module Data = Ocsforge_data
module Types = Ocsforge_types
module STypes = Ocsforge_source_types
module Vm = Ocsforge_version_managers

let create_repository_table_content ~sp ~id ~version = 
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let rec build_content tree current_dir = match tree with
	    | STypes.File(f,aut,rev)    ->
		(*let file_URL = 
		  if String.length current_dir == 0 then f
		  else current_dir^"/"^f
		in*)
		Lwt.return
                  {{ [ <tr>[
                         <td>{: f :}
		         <td>{: aut :}
		         <td>{: rev :}
		      ] ]
                   }} 
 	    | STypes.Dir (d, l) ->
		let rec aux list dir (res : {{ [ Xhtmltypes_duce.tr+ ] }}) = 
		  match list with
	            | []   -> Lwt.return res 
		    | h::t -> 
			build_content h dir >>= fun built ->
			  (aux t dir ({{ [ !res !built ] }}))
		in
		let new_dir = 
		  if (String.length current_dir == 0) then   
		    if (String.compare d "." != 0) then d
		    else ""
		  else (current_dir^"/"^d)
		in
		let a = 
		  {{ <tr>[
		     <td> {: new_dir :}
		   ]
		   }}
                in
                ((aux l new_dir {{ [ a ] }}) : {{ [ Xhtmltypes_duce.tr+ ] }} Lwt.t)
	  in
	  fun_pack.STypes.vm_list ~id:version path >>= fun tree -> 
	    ((build_content tree "") : {{ [ Xhtmltypes_duce.tr+ ] }} Lwt.t)
	
    | (_,_) -> Lwt.return {{ [ <tr> [
			       <td> ['Error: unable to access the repository']]] }}
     
  
let draw_repository_table ~sp ~id ~version =
  create_repository_table_content ~sp ~id ~version >>= fun b ->
    Lwt.return ({{ [<table> b] }} : {{ [ Xhtmltypes_duce.table ] }})
