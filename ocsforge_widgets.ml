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
let (!!) = Lazy.force

module Roles = Ocsforge_roles
module Types = Ocsforge_types
module Data  = Ocsforge_data


class add_task_widget (add_task_service) =
object (self)

  val add_task_classe = "ocsforge_add_task_form"

  method display ~sp ~parent ?(rows = 5) ?(cols = 50) () =
    let draw_form (*args ?*) =
      {{ 'Title :'
         {:Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
             ~name:(*TODO*) () :}
         'Description :'
         {:Eliom_duce.Xhtml.textarea ~name(*TODO*) ~rows ~cols () :}

      }}
    in
      Eliom_duce.Xhtml.get_form
        ~service:add_task_service
        ~sp draw_form ()
end



class display_task_widget (*display (and propose edition for rightful users) a task*)
        (edit_task_service) =
object (self)

  val task_class = "ocsforge_task_class"

  method display
    ~sp ~task () =
    let rec draw_field_value ~title ~value ?alternatives ~string_of_value () =
      let title_ = title ^ " : " in
      match value with
        | None ->
            begin
              match alternatives with
                | None | Some [] -> {{ }}
                | Some [alt] ->
                    {{
                       title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], alt, None, false)) 
                         []
                         string_of_value :}
                    }}
                | Some alt_hd::alt_tl ->
                    {{
                      title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], alt_hd, None, false)) 
                         (List.map (fun a -> ([],a,None,false)) alt_tl)
                         string_of_value :}
                    }}
            end
        | Some value ->
            begin
              match alternatives with
                | None | Some [] -> {{ title_ value }}
                | Some [alt] ->
                    {{ title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], value, None, true)) 
                         [(Eliom_duce.Xhtml.Optgroup
                             ([], "Alternative",
                              ([], a, None, false), []))]
                         string_of_value :}
                    }}
                | Some alt_hd::alt_tl ->
                    {{ title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], value, None, true))
                         [(Eliom_duce.Xhtml.Optgroup
                             ([], "Alternatives",
                              ([], alt_hd, None, false),
                              (List.map (fun a -> ([],a,None,false)) alt_tl)))]
                         string_of_value :}
                    }}
            end
    in
    let draw_rightfull_field ~right ~title ~value ~alternatives ~string_of_value
          () =
      if right
      then draw_field_value
             ~title ~value ~alternatives:(!!alternatives) ~string_of_value ()
      else draw_field_value
             ~title ~value ~string_of_value ()
    in
    Ocsforge_sql.get_task_by_id ~task_id:task    >>= fun info ->
    Roles.get_user_task_rights ~sp ~task_id:task >>= fun role ->
    !!(role.Roles.task_reader)                   >>= b ->
    if not b then {{ 'Permission denied' }} else
    !!(role.Roles.task_comment_reader)           >>= fun comment_reader ->
    !!(role.Roles.task_property_editor)          >>= fun prop_editor ->
      {{
        Forum_widget.display_message_widget#display_message
          ~sp ~message:info.Types.t_message
        {: Forum_widget.display_message_widget#display_message
                  ~sp ~message:info.Types.t_comments
        :}
        <p>[
          {: draw_rightfull_field
               ~right:prop_editor ~title:"kind" ~value:info.Types.t_kind
               ~alternatives:(
                 lazy (Ocsforge_sql.get_kinds_by_area
                         ~area_id:info.Types.t_area
                         ()) )
               ~string_of_value:(fun k -> k) ()
          :}
          {: draw_rightfull_field
               ~right:prop_editor ~title:"progress" ~value:info.Types.t_progress
               ~alternatives:(
                 lazy (Types.interval_list ~bump:5 ~min:0 ~max:100))
               ~string_of_value:Int32.to_string ()
          :}
          {: draw_rightfull_field
               ~right:prop_editor ~title:"importance"
               ~value:info.Types.t_importance
               ~alternatives:(
                 lazy (Types.interval_list ~bump:5 ~min:0 ~max:100))
               ~string_of_value:Int32.to_string ()
          :}
(*          {: draw_rightfull_field
               ~right:prop_editor ~title:"deadline (time)"
               ~value:info.Types.t_deadline_time
               ~alternatives:(
                 (*TODO*))
               ~string_of_value:(*TODO*) ()
          :}
          {: draw_rightfull_field
               ~right:prop_editor ~title:"deadline (version)"
               ~value:info.Types.t_deadline_version
               ~alternatives:(
                 (*TODO*))
               ~string_of_value:(*TODO*) ()
          :}*)
        ]
      }}
