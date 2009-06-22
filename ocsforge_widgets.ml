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
module FWidgets = Forum_widgets
module Lang = Ocsforge_lang

open CalendarLib


(** draw a select field for optional values.
* If a list of alternative is provided, it makes it selectable.*)
let draw_opt_field ?title ~name ~value ?alternatives ~string_of_t () =
  let print_opt_title = function
    | None -> ""
    | Some t -> t ^ " : "
  in
  match (value,alternatives) with
    | (None, None) | (None, Some []) ->
        {{ [ {: print_opt_title title :} {: "none" :} ] }}
    | (Some v, None) | (Some v, Some [])->
        {{ [ {: print_opt_title title :} {: string_of_t v :} ] }}
    | (None, Some l) ->
        {{ 
          [ {: print_opt_title title :}
            {:Eliom_duce.Xhtml.user_type_select
               (function
                  | None -> "None"
                  | Some v -> "\"" ^ (string_of_t v) ^ "\"")
                ~name
               (Eliom_duce.Xhtml.Option ({{ { } }}, None, None, true)) 
               (List.map
                  (fun a ->
                    Eliom_duce.Xhtml.Option ({{ { } }}, Some a, None, false))
                  l)
            :} ]
        }}
    | (Some v, Some l) ->
        {{ 
          [{: print_opt_title title :}
           {:Eliom_duce.Xhtml.user_type_select
             (function
                 | None -> "None"
                 | Some v -> "\"" ^ (string_of_t v) ^ "\"")
              ~name
              (Eliom_duce.Xhtml.Option ({{ { } }}, Some v, None, true))
              ( (List.map
                   (fun a ->
                      Eliom_duce.Xhtml.Option ({{ { } }}, Some a, None, false))
                   l)
               @ [Eliom_duce.Xhtml.Option ({{ { } }}, None, None, false)])
              
           :}]
        }}
let draw_field ?title ~name ~value ?alternatives ~string_of_t () =
  let print_opt_title = function
    | None -> ""
    | Some t -> t ^ " : "
  in
  match (value,alternatives) with
    | (v, None) ->
        {{ [{: print_opt_title title :} ' : ' {: string_of_t v :}] }}
    | (v, Some l) ->
        {{
          [{: print_opt_title title :} ' : '
           {:Eliom_duce.Xhtml.user_type_select
              string_of_t
              ~name
              (Eliom_duce.Xhtml.Option ({{ { } }}, v, None, true))
              (List.map
                 (fun a ->
                    Eliom_duce.Xhtml.Option ({{ { } }}, a, None, false))
                 l)
           :}]
        }}

let draw_simple_field ~name ~value ?alternatives ~string_of_t () =
         {{ <td>[
              {: draw_opt_field ~name ~value ?alternatives ~string_of_t :}
            ]
         }}

class add_task_widget (add_task_service) =
object (self)

  val add_task_classe = "ocsforge_add_task_form"

  method display ~sp ~parent ?(rows = 5) ?(cols = 50) () =
    Ocsforge_data.get_task ~sp ~task:parent >>= fun t_info ->
    Ocsforge_data.get_inheritance ~sp ~area:t_info.Types.t_area >>= fun inh ->
      let draw_form (progress_name,
                     (importance_name,
                      (kind_name,
                       (deadline_t_name,
                        (deadline_v_name,
                         (length_name,
                          detach_name     )))))) =
        Ocsforge_data.get_kinds ~sp ~area:inh >>= fun altk ->
        Lwt.return
        {{ (*TODO: show the message and comment depending on user rights*)
          (*'Title :'
           {:Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
               ~name:(*TODO*) () :}
           'Description :'
           {:Eliom_duce.Xhtml.textarea ~name:(*TODO*) ~rows ~cols () :}*)
        [
          {: 
             draw_opt_field
               ~title:"kind" ~name:kind_name ~value:None
               ~alternatives:altk
               ~string_of_t:(fun k -> k) ()
          :}
          {: draw_opt_field
               ~title:"progress" ~name:progress_name ~value:None
               ~alternatives:(Ocsforge_lang.int32_interval_list
                                ~bump:(Int32.of_int 5)
                                ~min:Int32.zero
                                ~max:(Int32.of_int 100)
                                ())
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"importance" ~name:importance_name ~value:None
               ~alternatives:(Ocsforge_lang.int32_interval_list
                                ~bump:(Int32.of_int 5)
                                ~min:Int32.zero
                                ~max:(Int32.of_int 100)
                                ())
               ~string_of_t:Int32.to_string ()
          :}
          {: draw_opt_field
               ~title:"deadline (time)" ~name:deadline_t_name ~value:None
               ~alternatives:(
                  (Ocsforge_lang.date_interval_list
                     ~min:(Calendar.now ())
                     ~max:(Calendar.add
                             (Calendar.now ())
                             (Calendar.Period.lmake ~day:7 ()))
                     () )
                 @(Ocsforge_lang.date_interval_list
                    ~bump:(Calendar.Period.lmake ~day:7 ())
                    ~min:(Calendar.add
                             (Calendar.now ())
                             (Calendar.Period.lmake ~day:7 ()))
                    ~max:(Calendar.add
                             (Calendar.now ())
                             (Calendar.Period.lmake ~month:1 ()))
                    ()
                  ))
               ~string_of_t:Printer.Calendar.to_string ()
          :}
          {: {{ [
                {: "deadline (version) : " :}
                {: (Eliom_duce.Xhtml.string_input
                     ~name:deadline_v_name
                     ~input_type:{: "text" :}
                     () ) :}
                ]
             }}
          :}
          {: draw_opt_field
               ~title:"length" ~name:length_name ~value:None
               ~alternatives:(
                   (Ocsforge_lang.period_interval_list
                      ~min:(Calendar.Period.lmake ~hour:1 ())
                      ~max:(Calendar.Period.lmake ~hour:6 ())
                      ())
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:12 ())
                      ~min:(Calendar.Period.lmake ~hour:12 ())
                      ~max:(Calendar.Period.lmake ~hour:24 ())
                      ())
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:24 ())
                      ~min:(Calendar.Period.lmake ~hour:48 ())
                      ~max:(Calendar.Period.lmake ~hour:96 ())
                      ()))
               ~string_of_t:Ocsforge_lang.string_of_period ()
          :}
        ]
 
        }}
      in
        Eliom_duce.Xhtml.lwt_get_form
          ~service:add_task_service
          ~sp draw_form

end


class tree_widget =
object (self)

  val task_text_class = "ocsforge_task_text"

  method private task_snippet ~sp ~width ~depth ~message =
    Forum_data.get_message ~sp ~message_id:message >>= fun msg ->
    Lwt.return (Lang.unopt ~default:"\"NONE\"" msg.Forum_sql.Types.m_subject)

  method display ~sp ~root_task ~fields ?(width = 600) ?(padding = 10)
           (progress_name,
            (importance_name,
             (kind_name,
              (deadline_t_name,
               (deadline_v_name,
                (length_name,
                 detach_name     ))))))=
    Data.get_tree ~sp ~root:root_task >>= fun tree ->
    let rec aux ~width ~depth =
    (function
       | Data.Nil -> Lwt.return {{ [] }} (*TODO: print "no task"*)
       | Data.Node (t, l) -> (
           self#task_snippet ~sp ~width ~depth
             ~message:t.Types.t_message >>= fun snip ->
           Data.get_kinds ~sp ~area:t.Types.t_area >>= fun alt_k -> Lwt.return
           {{ <tr>[
                <th>[ {: snip :} ]
                {: List.map
                     (function
                        | "progress" ->
                            draw_simple_field ~name:progress_name
                              ~value:t.Types.t_progress
                              ~alternatives:(Lang.int32_interval_list
                                               ~bump:(Int32.of_int 5)
                                               ~min:Int32.zero
                                               ~max(Int32.of_int 100) ())
                              ~string_of_t:Int32.to_string
                        | "importance" ->
                            draw_simple_field ~name:importance_name
                              ~value:t.Types.t_importance
                              ~alternatives:(Lang.int32_interval_list
                                               ~bump:(Int32.of_int 5)
                                               ~min:Int32.zero
                                               ~max(Int32.of_int 100) ())
                              ~string_of_t:Int32.to_string
                        | "kind" ->
                            draw_simple_field ~name:kind_name
                              ~value:t.Types.t_kind
                              ~alternatives:alt_k
                              ~string_of_t:(fun k -> k)
                        | "deadline_time" ->
                            draw_simple_field ~name:deadline_t_name
                              ~value:t.Types.t_deadline_time
                              ~alternatives:(
                                (Ocsforge_lang.date_interval_list
                                   ~min:(Calendar.now ())
                                   ~max:(Calendar.add
                                           (Calendar.now ())
                                           (Calendar.Period.lmake ~day:7 ()))
                                           () )
                                @(Ocsforge_lang.date_interval_list
                                    ~bump:(Calendar.Period.lmake ~day:7 ())
                                    ~min:(Calendar.add
                                            (Calendar.now ())
                                            (Calendar.Period.lmake ~day:7 ()))
                                    ~max:(Calendar.add
                                            (Calendar.now ())
                                            (Calendar.Period.lmake ~month:1 ()))
                                               ()
                                ))
                              ~string_of_t:Printer.Calendar.to_string ()
                        | "deadline_version" ->
                            {{ <td>[
                                 {: (Eliom_duce.Xhtml.string_input
                                       ~name:deadline_v_name
                                       ~input_type:{: "text" :}
                                       ?value:t.Types.t_deadline_version
                                 () ) :} ] }}
                        | "length" ->
                            draw_simple_field ~name:length_name
                              ~value:t.Types.t_length
                              ~alternatives:(
                                (Ocsforge_lang.period_interval_list
                                   ~min:(Calendar.Period.lmake ~hour:1 ())
                                   ~max:(Calendar.Period.lmake ~hour:6 ())
                                           ())
                                @(Ocsforge_lang.period_interval_list
                                    ~bump:(Calendar.Period.lmake ~hour:12 ())
                                    ~min:(Calendar.Period.lmake ~hour:12 ())
                                    ~max:(Calendar.Period.lmake ~hour:24 ())
                                            ())
                                @(Ocsforge_lang.period_interval_list
                                    ~bump:(Calendar.Period.lmake ~hour:24 ())
                                    ~min:(Calendar.Period.lmake ~hour:48 ())
                                    ~max:(Calendar.Period.lmake ~hour:96 ())
                                            ()))
                              ~string_of_t:Lang.string_of_period ()
                     )
                     fields
                :}
              ]

           }}
         )


    ) in aux ~width ~depth:0 tree
 


end


(*

class task_widget
  (*display (and, for granted users, propose edition of) a task*)
  (edit_task_service) =
object (self)

  (*TODO :
   * really use classes.
   * make a display method
   * make task movable when granted *)

  val task_class = "ocsforge_task_class"
  val editable_fields_class = "ocsforge_task_editable_fields"
  val fields_class = "ocsforge_task_fields"
  val message_class = "ocsforge_task_message"
  val comments_class = "ocsforge_task_comments"

  method private display_editable_fields ~sp ~task () =
      Ocsforge_data.get_task ~sp ~task >>= fun info ->
      let draw_form (id_name,
                     (progress_name,
                      (importance_name,
                       (kind_name,
                        (deadline_t_name,
                         (deadline_v_name,
                          (length_name,
                           detach_name     ))))))) =
      {{
        <p>[
          {: draw_opt_field
               ~title:"kind" ~name:kind_name ~value:info.Types.t_kind
               ~alternatives:(Ocsforge_sql.get_kinds_by_area
                                ~area_id:info.Types.t_area ())
               ~string_of_t:(fun k -> k) ()
          :}
        <br>[]
          {: draw_opt_field
               ~title:"progress" ~name:progress_name
               ~value:info.Types.t_progress
               ~alternatives:(Ocsforge_lang.int_interval_list
                                ~bump:5 ~min:0 ~max:100)
               ~string_of_t:Int32.to_string ()
          :}
        <br>[]
          {: draw_opt_field
               ~title:"importance" ~name:importance_name
               ~value:info.Types.t_importance
               ~alternatives:(
                 lazy (Ocsforge_lang.int_interval_list ~bump:5 ~min:0 ~max:100))
               ~string_of_t:Int32.to_string ()
          :}
        <br>[]
          {: draw_opt_field
               ~title:"deadline (time)" ~name:deadline_t_name
               ~value:info.Types.t_deadline_time
               ~alternatives:((Ocsforge_lang.date_interval_list ~min:1 ~max:7)
                             @(Ocsforge_lang.date_interval_list
                                ~bump:(Calendar.Period.lmake
                                         ~day:7 ())
                                ~min:1 ~max:(7 * 4)))
               ~string_of_t:Printer.Calendar.to_string ()
          :}
        <br>[]
          {: {{ {: "deadline (version) : " :}
                {: (Eliom_duce.Xhtml.string_input
                     ~input_type:{: "text" :}
                     ~value:info.Types.t_deadline_version)
                     ~name:deadline_v_name :}
             }}
          :}
        <br>[]
          {: draw_opt_field
               ~title:"length" ~name:length_name
               ~value:info.Types.length
               ~alternatives:(lazy (
                 ( (Ocsforge_lang.period_interval_list ~min:1 ~max:6)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:12 ())
                      ~min:12 ~max:24)
                  @(Ocsforge_lang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:24 ())
                      ~min:48 ~max:96))))
               ~string_of_t:(Ocsforge_lang.string_of_period) ()
          :}
        <br>[]
          {: {{ {: "Detach : " :}
                {: Eliom_duce.Xhtml.bool_checkbox ~name:detach_name () :}
             }}
          :}
        ]
      }}
      in Eliom_duce.Xhtml.get_form
           ~service:edit_task
           ~sp draw_form ()

  method private display_non_editable_fields ~sp ~task () =
      Ocsforge_data.get_task ~sp ~task >>= fun info ->
        {{ <p>[
             {:"kind : " :}               {: info.Roles.t_kind :}
               <br>[]
             {:"progress : " :}           {: info.Roles.t_progress :}
               <br>[]
             {:"importance : " :}         {: info.Roles.t_importance :}
               <br>[]
             {:"deadline (time) : " :}    {: info.Roles.t_deadline_time :}
               <br>[]
             {:"deadlien (version) : " :} {: info.Roles.t_deadline_version :}
               <br>[]
             {:"length : " :}             {: info.Roles.t_length :}
               <br>[]
        ]
        }}

  inherit FWidgets.message_widget
        (new Widget.widget_with_error_box)
        (Wiki_models.get_widgets forge_wiki_model)
        ((*Needs to get forums services*)) as message_widget

  method display ~sp ?(classes = []) id () =
    let task = id in
    Data.get_task ~sp ~task >>= fun info ->
    Roles.get_area_role ~sp info.Types.t_area   >>= fun role ->
    !!(role.Roles.task_reader)                  >>= fun reader ->
    !!(role.Roles.task_comment_reader)          >>= fun c_reader ->
    !!(role.Roles.task_property_editor)         >>= fun prop_editor ->
      (if reader
       then
         Lwt.return
           {{ <div class={: message_class :}>
                 [ {: message_widget#display
                        ~sp ~classes ~data:info.Types.t_message ()
                   :} ] }}
       else Lwt.return {{ [] }}               ) >>= fun msg ->
      (if c_reader
       then
         Lwt.return
           {{ <div class={: comments_class :}>[(*TODO : display_comments*)] }}
       else Lwt.return {{ [] }}               ) >>= fun cmt ->
      (if reader
       then
         if prop_editor
         then
           Lwt.return
              {{ <div class={: editable_fields_class :}>
                  [ {: self#display_editable_fields ~sp ~task () :} ] }}
         else
           Lwt.return
              {{ <div class={: fields_class :}>
                  [ {: self#display_non_editable_fields ~sp ~task () :} ] }}
       else Lwt.return {{ [] }}               ) >>= fun fld ->
 
       Lwt.return {{ <div class={: classes :}>[ msg cmt fld ] }}

end

       *)

class view_tasks = object end
class view_tree = object end
class edit_area = object end
class view_task_history = object end
