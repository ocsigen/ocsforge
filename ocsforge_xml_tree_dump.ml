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
module Olang = Ocsforge_lang
let to_utf8 = Ocamlduce.Utf8.make
let (>>=) = Lwt.bind

type boolean = {{ "true" | "false" }}
let boolean_of_bool b = if b then {{ "true" }} else {{ "false" }}
type task_attrs = (*TODO: improve type checking*)
    {{ {
         id         =? String
         length     =? String
         progress   =? String
         importance =? String
         deadline   =? String
         milestone  =? String
         kind       =? String
         deleted    =? boolean
    } }}
type xml_task =
    {{ <task (task_attrs) >
         [ Ocamlduce.Load.anyxml
           <children>[ xml_task* ]
         ]
    }}
type xml_task_tree = {{ <task_tree>[ xml_task ] }}

let rec xml_task_of_tree ~sp inline_widget (*TODO : use all fields ? *)
  { Types.Tree.content =
      { Types.t_id = id                ; Types.t_message = msg       ;
        Types.t_length = len           ; Types.t_progress = pro      ;
        Types.t_importance = imp       ; Types.t_deadline_time = dea ;
        Types.t_deadline_version = mil ; Types.t_kind = kin          ;
        Types.t_area = area            ; Types.t_deleted = del       ;
      } ;
    Types.Tree.children =  l ;
  }
   =
  let s_of_i32_opt = Olang.string_of_t_opt Int32.to_string in

  Ocsforge_widgets_tasks.draw_message_title ~sp ~message:msg inline_widget
                                                        >>= fun msg -> 
  Lwt_util.map_serial (xml_task_of_tree ~sp inline_widget) l >>= fun l ->
  Lwt.return
    ({{ <task id         = {: to_utf8 (Types.string_of_task id)      :}
              length     = {: to_utf8 (Olang.string_of_t_opt
                                         Olang.string_of_period len) :}
              progress   = {: to_utf8 (s_of_i32_opt pro)             :}
              importance = {: to_utf8 (s_of_i32_opt imp)             :}
              deadline   = {: to_utf8 (Olang.string_of_t_opt
                                         Olang.string_of_date dea)   :}
              milestone  = {: to_utf8 (Olang.string_of_t_opt
                                         (fun m -> m) mil)           :}
              kind       = {: to_utf8 (Olang.string_of_t_opt
                                         (fun k -> k) kin)           :}
              deleted    = {{ boolean_of_bool del }}
          >[ msg <children>[ !{: l :} ] ]
      }} : {{ xml_task }} )


let xml_of_tree ~sp il_widget tree =
  xml_task_of_tree ~sp il_widget tree >>= fun tasks ->
  Lwt.return {{ <task_tree>[ tasks ] }}
