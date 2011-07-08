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

exception Manager_not_supported
exception Manager_command_error
exception Revision_not_found
exception Node_not_found
exception Wrong_node_kind

(* List of supported version management systems *)
let fun_list:((string * Ocsforge_source_types.fun_pack) list ref) = ref []

let set_fun_pack vm_name fun_pack = 
  try 
    let _ = List.assoc vm_name (!fun_list) in
    ()
  with Not_found ->
    fun_list := ((vm_name,fun_pack)::(!fun_list))

let get_fun_pack vm_name = 
  Lwt.catch 
    (fun () -> Lwt.return (List.assoc vm_name (!fun_list)))
    (function | _ -> Lwt.fail Manager_not_supported)

let get_managers_list () = fst (List.split (!fun_list))

let pair_to_string p =
  if (snd p == -1) then
    fst p 
  else
    ((fst p)^":"^(Int32.to_string (Int32.of_int (snd p))))

let string_to_pair s =
  let l = Netstring_pcre.split (Netstring_pcre.regexp ":") s 
  in match l with
    | [hash;pos] -> (hash,Int32.to_int (Int32.of_string pos))
    | [hash] -> (hash,-1)
    | _ -> ("",-1)

let range_to_string r = match r with
  | (None,None) -> ""
  | (None,Some(end_rev)) -> (":"^end_rev)
  | (Some(start_rev),None) -> (start_rev^":")
  | (Some(start_rev),Some(end_rev)) -> (start_rev^":"^end_rev)

let string_to_range s = 
  let l = Netstring_pcre.split_delim (Netstring_pcre.regexp ":") s
  in match l with
    | ["";end_rev] -> (None,Some(end_rev))
    | [start_rev;""] -> (Some(start_rev),None)
    | [start_rev; end_rev] -> (Some(start_rev),Some(end_rev))
    | _ -> (None,None)
  
