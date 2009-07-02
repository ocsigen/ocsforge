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

exception Manager_not_supported

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
