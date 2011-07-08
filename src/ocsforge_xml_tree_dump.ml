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

(** @author Raphael Proust*)


module Types = Ocsforge_types
module Tree = Ocsforge_lang.Tree
module Olang = Ocsforge_lang
module Roles = Ocsforge_roles
open Eliom_pervasives
let (@@) f g = fun x -> f (g x)

open XML

let rec xml_of_tree ~task ?depth ?with_deleted () =
  let s_of_i32_opt = Olang.string_of_t_opt Int32.to_string in
    let rec aux_tree
    { Tree.content =
        { Types.t_id = id          ; 
          Types.t_length = len     ; Types.t_progress = pro ;
          Types.t_importance = imp ; Types.t_kind = kin     ;
          Types.t_area = area      ; Types.t_deleted = del  ;
          Types.t_area_root = ar   ;
        } ;
      Tree.children = l ; }
    =
    Ocsforge_widgets_tasks.draw_message_title ~task:id     >>= fun msg  -> (*FIXME: get the title in a cleaner way*)
    Roles.get_area_role area                               >>= fun role ->
    Lazy.force ( role.Roles.task_property_editor )         >>= fun edi  ->
    Lazy.force ( role.Roles.task_mover )                   >>= fun mov  ->
    Lwt_util.map_serial aux_tree l                         >>= fun l ->
    Lwt.return
      (node "task"
	 ~a:((List.map (fun (n,v) -> string_attrib n v)
		["id",Types.string_of_task id;
		 "length",(Olang.string_of_t_opt
                             (string_of_int @@ Olang.hours_in_period)
                             len);
		 "progress",(s_of_i32_opt pro);
		 "importance",(s_of_i32_opt imp);
                 "kind",(Olang.string_of_t_opt (fun k -> k) kin);])
	       @
		 (List.map (fun (n,b) -> string_attrib n (string_of_bool b))
		    ["deleted",del;
		     "editable", edi;
		     "movable", mov;
		     "project", ar;]))
	 [node "subject" [pcdata msg];
	  node "children" l])
    in
    let rec aux_sep = function
      | [] -> []
      | h::t ->
	let t = aux_sep t in
	(node "separator"
	    ~a:[string_attrib "id" (Types.string_of_separator h.Types.s_id);
		string_attrib "after" (Types.string_of_task h.Types.s_after)]
	    [pcdata h.Types.s_content])::t
    in
    try_lwt
      Ocsforge_data.get_tree ~root:task ?with_deleted ?depth ()
      >>= aux_tree >>= fun c ->

      Ocsforge_data.get_separators ~task
      >>= Lwt.return @@ aux_sep >>= fun s ->

      Lwt.return ( node "task_tree" [ node "seps" s; c ] )
    with
      | Tree.Empty_tree -> Lwt.return ( leaf "task_tree" )
      | exc -> Lwt.fail exc

