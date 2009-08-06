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

module Types = Ocsforge_types
module Tree = Ocsforge_lang.Tree
module Olang = Ocsforge_lang
module Roles = Ocsforge_roles
let to_utf8 = Ocamlduce.Utf8.make
let (>>=) = Lwt.bind

(* boolean type for ocamlduce *)
type boolean = {{ "true" | "false" }}
let boolean_of_bool b = if b then {{ "true" }} else {{ "false" }}

(* The separators *)
type separator = {{ <separator id=String after=String>[ ( Char )* ] }}
type separators ={{ <seps>[ ( separator )* ] }}

(* The tasks *)
type task_attrs = (*TODO: improve type checking*)
    {{ {
                 id =? String  (* the id of the task          *)
               kind =? String  (* the category the task is in *)
           _length_ =? String  (* the estimated length        *)
           progress =? String  (* the progress made           *)
         importance =? String  (* the importance              *)

          deleted =? boolean (* true if the task has been deleted  *)
         editable =? boolean (* true if edition rights are granted *)
          movable =? boolean (* true if the user is a task_mover   *)
          project =? boolean (* true if the task is a project      *)
    } }}
type xml_task =
    {{ <task (task_attrs) >
         [
           <subject>[ Char* ]
           <children>[ xml_task* ]
         ]
    }}

(* The whole format *)
type dump_format = {{ <task_dump>[ ( separators xml_task )? ] }}



let rec xml_of_tree ~sp ~task ?depth ?with_deleted () =
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
    Ocsforge_widgets_tasks.draw_message_title ~sp ~task:id >>= fun msg  ->
    Roles.get_area_role sp area                            >>= fun role ->
    Lazy.force ( role.Roles.task_property_editor )         >>= fun edi  ->
    Lazy.force ( role.Roles.task_mover )                   >>= fun mov  ->
    Lwt_util.map_serial aux_tree l                         >>= fun l ->
    Lwt.return
      ({{ <task id         = {: to_utf8 (Types.string_of_task id)      :}
                _length_   = {: to_utf8 (Olang.string_of_t_opt
                                           string_of_int
                                          (Olang.apply_on_opted
                                             Olang.hours_in_period
                                             len))                    :}
               progress   = {: to_utf8 (s_of_i32_opt pro)             :}
               importance = {: to_utf8 (s_of_i32_opt imp)             :}
               kind       = {: to_utf8 (Olang.string_of_t_opt
                                          (fun k -> k) kin)           :}
               deleted    = {{ boolean_of_bool del }}
               editable   = {{ boolean_of_bool edi }}
               movable    = {{ boolean_of_bool mov }}
               project    = {{ boolean_of_bool ar  }}
          >[
            <subject>[ !{: to_utf8 msg :} ]
            <children>[ !{: l :} ]
           ]
       }} : {{ xml_task }} )
  in
  let rec aux_sep = function
    | [] -> ({{ [ ] }} : {{ [ separator* ] }})
    | h::t -> let t = aux_sep t in
        ({{ [
           <separator id    = {: to_utf8
                                   (Types.string_of_separator h.Types.s_id)
                              :}
                      after = {: to_utf8
                                (Types.string_of_task h.Types.s_after)
                              :}
           > {: to_utf8 h.Types.s_content :} !t
         ] }} : {{ [ separator+ ] }} )
  in

  Ocsforge_data.get_separators ~sp ~task          >>= fun seps ->
  Lwt.return (aux_sep seps)                       >>= fun seps ->
  (Lwt.catch
     (fun () ->
        Ocsforge_data.get_tree ~sp ~root:task ?with_deleted ?depth ()
        >>= aux_tree >>= fun c ->
        Lwt.return {{ <task_dump>[ <seps> seps c ] }}
     )
     (function
        | Tree.Empty_tree -> Lwt.return {{ <task_dump>[] }}
        | exc -> Lwt.fail exc) )






