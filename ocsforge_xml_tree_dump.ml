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
let utf8 = Ocamlduce.Utf8.make
let (>>=) = Lwt.bind
let (@@) f g = fun x -> f (g x)


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
type task =
    {{ <task (task_attrs) >
         [
           <subject>[ Char* ]
           <children>[ task* ]
         ]
    }}

(* The whole format *)
type dump_format = {{ <task_tree>[ ( separators task )? ] }}


(** Get the task tree and return a [dump_format] xml tree. *)
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
    Ocsforge_widgets_tasks.draw_message_title ~sp ~task:id >>= fun msg  -> (*FIXME: get the title in a cleaner way*)
    Roles.get_area_role sp area                            >>= fun role ->
    Lazy.force ( role.Roles.task_property_editor )         >>= fun edi  ->
    Lazy.force ( role.Roles.task_mover )                   >>= fun mov  ->
    Lwt_util.map_serial aux_tree l                         >>= fun l ->
    Lwt.return
      ({{ <task id         = {: utf8 (Types.string_of_task id)      :}
                _length_   = {: utf8
                               (Olang.string_of_t_opt
                                  (string_of_int @@ Olang.hours_in_period)
                                  len
                               )                                    :}
               progress   = {: utf8 (s_of_i32_opt pro)              :}
               importance = {: utf8 (s_of_i32_opt imp)              :}
               kind       = {: utf8 (Olang.string_of_t_opt
                                          (fun k -> k) kin)         :}
               deleted    = {{ boolean_of_bool del }}
               editable   = {{ boolean_of_bool edi }}
               movable    = {{ boolean_of_bool mov }}
               project    = {{ boolean_of_bool ar  }}
          >[
            <subject>[ !{: utf8 msg :} ]
            <children>[ !{: l :} ]
           ]
       }} : {{ task }} )
  in
  let rec aux_sep = function
    | [] -> ({{ [ ] }} : {{ [ separator* ] }})
    | h::t -> let t = aux_sep t in
        ({{ [
           <separator id    = {: utf8
                                   (Types.string_of_separator h.Types.s_id)
                              :}
                      after = {: utf8
                                (Types.string_of_task h.Types.s_after)
                              :}
           > {: utf8 h.Types.s_content :} !t
         ] }} : {{ [ separator+ ] }} )
  in

  (Lwt.catch
     (fun () ->

        Ocsforge_data.get_tree ~sp ~root:task ?with_deleted ?depth ()
        >>= aux_tree >>= fun c ->

        Ocsforge_data.get_separators ~sp ~task
        >>= Lwt.return @@ aux_sep >>= fun s ->

        Lwt.return {{ <task_tree>[ <seps>s c ] }}
     )
     (function
        | Tree.Empty_tree -> Lwt.return {{ <task_tree>[] }}
        | exc -> Lwt.fail exc ) )

