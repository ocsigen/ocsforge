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

let draw_opt_field ~title ~name ~value ?alternatives ~string_of_t () =
  match (value,alternatives) with
    | (None, None) ->
        {{ {: title :} ' : none' }}
    | (Some v, None) ->
        {{ {: title :} ' : ' {: string_of_t v :} }}
    | (None, Some l) ->
        {{ {: title :} ' : '
           {:Eliom_duce.Xhtml.user_type_select ~name
              (Eliom_duce.Xhtml.Option ([], None, None, true)) 
              (List.map (fun a -> ([], Some a, None, false)) l)
              (function
                 | None -> "None"
                 | Some v -> "\"" ^ (string_of_t v) ^ "\""):}
        }}
    | (Some v, Some l) ->
        {{ {: title :} ' : '
           {:Eliom_duce.Xhtml.user_type_select ~name
              (Eliom_duce.Xhtml.Option ([], Some v, None, true))
              ( (List.map (fun a -> ([], Some a, None, false)) l)
               @([], None, None, false))
              (function
                 | None -> "None"
                 | Some v -> "\"" ^ (string_of_t v) ^ "\"")
           :}
        }}
let draw_field ~title ?name ~value ?alternatives ~string_of_t () =
  match (value,alternatives) with
    | (v, None) ->
        {{ {: title :} ' : ' {: string_of_t v :} }}
    | (v, Some l) ->
        {{ {: title :} ' : '
           {:Eliom_duce.Xhtml.user_type_select ~name
              (Eliom_duce.Xhtml.Option ([], v, None, true))
              (List.map (fun a -> ([], a, None, false)) l)
              string_of_t
           :}
        }}




class add_task_widget (add_task_service) =
object (self)

  val add_task_classe = "ocsforge_add_task_form"

  method display ~sp ~parent ?(rows = 5) ?(cols = 50) () =
    Ocsforge_sql.get_area_for_task ~task_id:task >>= fun a_info ->
    Roles.get_area_role ~sp a_info.Types.r_id    >>= fun role ->
    !!(role.Roles.task_creator)                  >>= fun right ->
    if right
    then
      let draw_form (progress_name,
                     (importance_name,
                      (kind_name,
                       (deadline_t_name,
                        (deadline_v_name,
                         (length_name,
                          detach_name     )))))) =
        {{ (*TODO: show the message and comment depending on user rights*)
          (*'Title :'
           {:Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
               ~name:(*TODO*) () :}
           'Description :'
           {:Eliom_duce.Xhtml.textarea ~name:(*TODO*) ~rows ~cols () :}*)
        <p>[
          {: draw_opt_field
               ~title:"kind" ~name:kind_name ~value:None
               ~alternatives:(Ocsforge_sql.get_kinds_by_area
                               ~area_id:info.Types.t_area ())
               ~string_of_t:(fun k -> k) ()
          :}
          {: draw_opt_field
               ~title:"progress" ~name:progress_name ~value:None
               ~alternatives:(Ocsforge_lang.int_interval_list
                                ~bump:5 ~min:0 ~max:100)
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"importance" ~name:importance_name ~value:None
               ~alternatives:(Ocsforge_lang.int_interval_list
                                ~bump:5 ~min:0 ~max:100)
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"deadline (time)" ~name:deadline_t_name ~value:None
               ~alternatives:(
                  (Ocsforge_lang.date_interval_list ~min:1 ~max:7)
                 @(Ocsforge_lang.date_interval_list
                    ~bump:(CalendarLib.Calendar.Period.lmake ~day:7 ())
                    ~min:1 ~max:(7 * 4)))
               ~string_of_t:CalendarLib.Printer.Calendar.to_string ()
          :}
          {: {{ {: "deadline (version) : " :}
                (Eliom_duce.Xhtml.string_input
                   ~name:deadline_v_name
                   ~input_type:{: "text" :}
                   ~value:info.Types.t_deadline_version
                   () )
             }}
          :}
          {: draw_opt_field
               ~title:"length" ~name:length_name ~value:None
               ~alternatives:(
                   (Ocsforge_lang.period_interval_list ~min:1 ~max:6)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(CalendarLib.Calendar.Period.lmake ~hour:12 ())
                      ~min:12 ~max:24)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(CalendarLib.Calendar.Period.lmake ~hour:24 ())
                      ~min:48 ~max:96))
               ~string_of_t:Ocsforge_lang.string_of_period ()
          :}
        ]
 
        }}
      in
        Eliom_duce.Xhtml.get_form
          ~service:add_task_service
          ~sp draw_form ()

    else {{ 'Permission denied' }}
end



class display_task_for_editing_widget
  (*display (and propose edition for granted users) a task*)
  (edit_task_service) =
object (self)

  val task_class = "ocsforge_task_class"

  method display
    ~sp ~task () =
    Ocsforge_sql.get_area_for_task ~task_id:task >>= fun a_info ->
    Roles.get_area_role ~sp a_info.Types.r_id    >>= fun role ->
    !!(role.Roles.task_property_editor)          >>= fun prop_editor ->
    if not b then {{ 'Permission denied' }} else
      let draw_form (id_name,
                     (progress_name,
                      (importance_name,
                       (kind_name,
                        (deadline_t_name,
                         (deadline_v_name,
                          (length_name,
                           detach_name     ))))))) =
      {{
        (*TODO:display message (and comments for granted user)*)
        (*Forum_widget.display_message_widget#display_message (*FIXME: need new*)
          ~sp ~message:info.Types.t_message
        {:if comment_reader
          then Forum_widget.display_message_widget#display_message
                  ~sp ~message:info.Types.t_comments
         else ''*)
        :}
        <p>[
          {: draw_opt_field
               ~title:"kind" ~name:kind_name ~value:info.Types.t_kind
               ~alternatives:(Ocsforge_sql.get_kinds_by_area
                                ~area_id:info.Types.t_area ())
               ~string_of_t:(fun k -> k) ()
          :}
          {: draw_opt_field
               ~title:"progress" ~name:progress_name
               ~value:info.Types.t_progress
               ~alternatives:(Ocsforge_lang.int_interval_list
                                ~bump:5 ~min:0 ~max:100)
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"importance" ~name:importance_name
               ~value:info.Types.t_importance
               ~alternatives:(
                 lazy (Ocsforge_lang.int_interval_list ~bump:5 ~min:0 ~max:100))
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"deadline (time)" ~name:deadline_t_name
               ~value:info.Types.t_deadline_time
               ~alternatives:((Ocsforge_lang.date_interval_list ~min:1 ~max:7)
                             @(Ocsforge_lang.date_interval_list
                                ~bump:(CalendarLib.Calendar.Period.lmake
                                         ~day:7 ())
                                ~min:1 ~max:(7 * 4)))
               ~string_of_t:CalendarLib.Printer.Calendar.to_string ()
          :}
          {: {{ {: "deadline (version) : " :}
                (Eliom_duce.Xhtml.string_input
                   ~input_type:{: "text" :}
                   ~value:info.Types.t_deadline_version)
                   ~name:deadline_v_name
             }}
          :}
          {: draw_opt_field
               ~title:"length" ~name:length_name
               ~value:info.Types.length
               ~alternatives:(lazy (
                 ( (Ocsforge_lang.period_interval_list ~min:1 ~max:6)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(CalendarLib.Calendar.Period.lmake ~hour:12 ())
                      ~min:12 ~max:24)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(CalendarLib.Calendar.Period.lmake ~hour:24 ())
                      ~min:48 ~max:96))))
               ~string_of_t:(Ocsforge_lang.string_of_period) ()
          :}
          {: {{ {: "Detach : " :}
                Eliom_duce.Xhtml.bool_checkbox ~name:detach_name ()
             }}
          :}
        ]
      }}
      in Eliom_duce.Xhtml.get_form
           ~service:edit_task
           ~sp draw_form ()

end
