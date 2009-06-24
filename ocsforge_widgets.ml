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
module Services = Ocsforge_services

open CalendarLib


(*
(** draw a select field for optional values.
* If a list of alternative is provided, it makes it selectable.*)
let draw_opt_field ?title ~name ~value ?alternatives ~string_of_t () =
  let print_opt_title = function
    | None -> ""
    | Some t -> t ^ " : "
  in
  let t = Ocamlduce.Utf8.make (print_opt_title title) in
  match (value,alternatives) with
    | (None, None) | (None, Some []) ->
        {{ [  !t !{: Ocamlduce.Utf8.make "none" :} ] }}
    | (Some v, None) | (Some v, Some [])->
        {{ [ !t !{: Ocamlduce.Utf8.make (string_of_t v) :} ] }}
    | (None, Some l) ->
        {{ 
          [ !t
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
          [ !t
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
  let t = Ocamlduce.Utf8.make (print_opt_title title) in
  match (value,alternatives) with
    | (v, None) ->
        {{ [ !t !{: Ocamlduce.Utf8.make (string_of_t v) :}] }}
    | (v, Some l) ->
        {{
          [ !t
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
 *)

let draw_savable_field ~sp ~service ~id ~value ~string_of_t ~alts () =
  Eliom_duce.Xhtml.post_form
    ~service ~sp
    (fun (id_name, param_name) ->
       match (value,alts) with
         | (None,   []) ->
             {{ [ <p>[ !{: Ocamlduce.Utf8.make "none" :}  ]] }}
         | (Some v, [])->
             {{ [ <p>[ !{: Ocamlduce.Utf8.make (string_of_t v) :} ] ] }}
         | (None,   l) ->
             {{ [
                <p>[
                 {: Eliom_duce.Xhtml.user_type_select
                      (Ocsforge_lang.string_of_t_opt string_of_t)
                      ~name:param_name
                      (Eliom_duce.Xhtml.Option ({{ { } }}, None, None, true)) 
                      (List.map
                         (fun a -> Eliom_duce.Xhtml.Option
                                     ({{ { } }}, Some a, None, false)
                         )
                         l)
                 :}
                 {: Eliom_duce.Xhtml.user_type_button
                      ~name:id_name
                      ~value:id
                      Types.string_of_task
                      {{ "Save" }}
                 :}
                ]
               ]
             }}
         | (Some v, l) ->
             {{ [
                <p>[
                 {:Eliom_duce.Xhtml.user_type_select
                     (Ocsforge_lang.string_of_t_opt string_of_t)
                     ~name:param_name
                     (Eliom_duce.Xhtml.Option ({{ { } }}, Some v, None, true))
                     ( (List.map
                         (fun a -> Eliom_duce.Xhtml.Option
                                    ({{ { } }}, Some a, None, false))
                         l)
                      @[Eliom_duce.Xhtml.Option ({{ { } }}, None, None, false)])

                 :}
                 {: Eliom_duce.Xhtml.user_type_button
                      ~name:id_name
                      ~value:id
                      Types.string_of_task
                      {{ "Save" }}
                 :}
                ]
               ]
             }}
    )
    ()


(*
class add_task_widget (add_task_service) =
object (self)

  val add_task_classe = "ocsforge_add_task_form"
  val fields_class = "ocsforge_task_fields"
  val message_class = "ocsforge_task_message"

  method display ~sp ~parent
           ~(add_message_widget:Forum_widgets.add_message_widget) () =
    Ocsforge_data.get_task ~sp ~task:parent >>= fun t_info ->
    Ocsforge_data.get_area ~sp ~area:t_info.Types.t_area >>= fun a_info ->
      let draw_form (subject_name,
                     (text_name,
                      (progress_name,
                       (importance_name,
                        (kind_name,
                         (deadline_t_name,
                          (deadline_v_name,
                           (length_name,
                            (detach_name,
                             save_name     ))))))))) =
        Ocsforge_data.get_kinds ~sp ~area:a_info.Types.r_inheritance >>= fun altk ->
        Lwt.return
        {{
          <div class={: add_task_classe :}>[
            <div class={: message_class :}>[
               'Title:'
               {: Eliom_duce.Xhtml.string_input ~input_type:{: "text" :}
                    ~name:subject_name () :}
               <br>[]
               {: Eliom_duce.Xhtml.textarea ~name:text_name
                    ~rows:5 ~cols:50 () :}
            ]
           
            <div class={: fields_class :}>[
            !{: draw_opt_field
                 ~title:"kind" ~name:kind_name ~value:None
                 ~alternatives:altk
                 ~string_of_t:(fun k -> k) ()
            :}
               <br>[]
            !{: draw_opt_field
                 ~title:"progress" ~name:progress_name ~value:None
                 ~alternatives:(Ocsforge_lang.int32_interval_list
                                  ~bump:(Int32.of_int 5)
                                  ~min:Int32.zero
                                  ~max:(Int32.of_int 100)
                                  ())
                 ~string_of_t:Int32.to_string ()
            :}
               <br>[]
            !{: draw_opt_field
                 ~title:"importance" ~name:importance_name ~value:None
                 ~alternatives:(Ocsforge_lang.int32_interval_list
                                  ~bump:(Int32.of_int 5)
                                  ~min:Int32.zero
                                  ~max:(Int32.of_int 100)
                                  ())
                 ~string_of_t:Int32.to_string ()
            :}
               <br>[]
            !{: draw_opt_field
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
               <br>[]
            !{: {{ [
                  'deadline (version) : '
                  {: (Eliom_duce.Xhtml.string_input
                       ~name:deadline_v_name
                       ~input_type:{: "text" :}
                       () ) :}
                  ]
               }}
            :}
               <br>[]
            !{: draw_opt_field
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
            <p>[
              {: Eliom_duce.Xhtml.string_button ~name:save_name
                   ~value:"save" {{ "Save" }} :}
            ]
          ] 
        }}
      in
        Eliom_duce.Xhtml.lwt_post_form
          ~service:add_task_service
          ~sp draw_form ()
end
 *)


class tree_widget =
object (self)

  val task_text_class = "ocsforge_task_text"

  method style_tag ~max_depth ~padding =
    {{
       <style type={: Ocamlduce.Utf8.make "text/css" :}>[
         !{:
             List.map
               (fun p -> "#depth" ^ (string_of_int p)
                       ^ "{ padding-left:"
                       ^ (string_of_int (padding * p)) ^ "px; }\n")
               (Ocsforge_lang.int_interval_list
                  ~bump:padding ~min:0 ~max:max_depth () )
          :}
       ]
    }}

  method private header ~fields =
    let field_count = List.length fields in
    ({{ [
       <colgroup>[
         <col>[] (*the task column*)
         !{:
           List.map
            (fun s -> {{ <col width={: (string_of_int (60 / field_count))
                                       ^ "%"
                                    :}>[] }} )
            fields
           (*There's no division by zero problem because the function is
            * called only when the list isn't empty*)
          :}
       ]
       <thead>[
         <tr>[
           <th>[ !{: Ocamlduce.Utf8.make "tasks" :} ]
           !{:
              List.map
               (fun s ->
                  {{ <td>[ (*TODO : better...  (filter fields ???)*)
                       !{: Ocamlduce.Utf8.make (if s = "" then " " else s) :}
                     ]
                  }} )
               fields
           :}
         ]
       ] ]
     }} : {{ [ Xhtmltypes_duce.colgroup Xhtmltypes_duce.thead ] }} )

  method private task_snippet ~sp ~width ~depth ~padding ~char_size ~message =
    Forum_data.get_message ~sp ~message_id:message >>= fun msg ->
    let shorten str dep wid pad siz =
      let max_size = (wid - (dep * pad)) / siz in
      if String.length str > max_size
      then String.sub str 0 max_size
      else str
    in
    let res =
      shorten
        (Ocsforge_lang.unopt ~default:"\"NONE\"" msg.Forum_sql.Types.m_subject)
        depth width padding char_size
    in
    Lwt.return res

  method private show_static_field ~field ~task:t = match field with
    | "progress" ->
        {{ <td>[
             !{: Ocsforge_lang.string_of_t_opt
                   Int32.to_string
                   t.Types.t_progress :} ] }}
    | "importance" ->
        {{ <td>[
            !{: Ocsforge_lang.string_of_t_opt
                  Int32.to_string
                  t.Types.t_importance :} ] }}
    | "kind" ->
        {{ <td>[ 
             !{: Ocsforge_lang.string_of_t_opt
                     (fun k -> k)
                     t.Types.t_kind :} ] }}
    | "deadline_time" ->
        {{ <td>[
             !{: Ocsforge_lang.string_of_t_opt
                   Printer.Calendar.to_string
                   t.Types.t_deadline_time :} ] }}
    | "deadline_version" ->
        {{ <td>[
             !{: Ocsforge_lang.string_of_t_opt
                   (fun d -> d)
                   t.Types.t_deadline_version :} ] }}
    | "length" ->
        {{ <td>[
            !{: Ocsforge_lang.string_of_t_opt
                  Ocsforge_lang.string_of_period
                  t.Types.t_length :} ] }}
    | f ->
      {{ <td>[  ] }}


  method private show_editable_field ~sp ~task:t alt_k = function 
    | "progress" ->
        draw_savable_field ~sp ~service:Services.set_progress_service
          ~id:t.Types.t_id
          ~value:t.Types.t_progress
          ~string_of_t:Int32.to_string
          ~alts:(Ocsforge_lang.int32_interval_list
                   ~bump:(Int32.of_int 5)
                   ~min:Int32.zero
                   ~max:(Int32.of_int 100) ())
          ()
    | "importance" ->
        draw_savable_field ~sp ~service:Services.set_importance_service
          ~id:t.Types.t_id
          ~value:t.Types.t_importance
          ~alts:(Ocsforge_lang.int32_interval_list
                           ~bump:(Int32.of_int 5)
                           ~min:Int32.zero
                           ~max:(Int32.of_int 100) ())
          ~string_of_t:Int32.to_string
          ()
    | "kind" ->
        draw_savable_field ~sp ~service:Services.set_kind_service
          ~id:t.Types.t_id
          ~value:t.Types.t_kind
          ~alts:alt_k
          ~string_of_t:(fun s -> s)
          ()
    | "deadline_time" ->
        draw_savable_field ~sp ~service:Services.set_deadline_time_service
          ~id:t.Types.t_id
          ~value:t.Types.t_deadline_time
          ~alts:(
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
          ~string_of_t:Printer.Calendar.to_string
          ()
(*    | "deadline_version" ->
        let draw_dlv (id_name, deadline_v_name) =
        {{ [
            {: Eliom_duce.Xhtml.string_input
                 ~name:deadline_v_name
                 ~input_type:{: "text" :}
                 ?value:t.Types.t_deadline_version
                 () :}
            {: Eliom_duce.Xhtml.user_type_button
                 ~name:id_name
                 ~value:t.Types.t_id
                 Types.string_of_task
                 {{ "Save" }}
            :} ]
        }}
        in Eliom_duce.Xhtml.post_form
             ~a:{{ { accept-charset="utf-8" } }}
             ~service:Services.set_deadline_version_service
             ~sp draw_dlv () *)
    | "length" ->
        draw_savable_field ~sp ~service:Services.set_length_service
          ~id:t.Types.t_id
          ~value:t.Types.t_length
          ~alts:(
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
                ()) )
            ~string_of_t:Ocsforge_lang.string_of_period
            ()
    | f ->
        let f = Ocamlduce.Utf8.make "unknown" in
          {{ <p>[ !f ] }}

  method display ~sp ~root_task ~fields
        ?(width = 600) ?(padding = 10) ?(char_size = 12)
        () =
    Data.get_tree ~sp ~root:root_task >>= fun tree ->
      let show_line ~width ~depth ~task:t =
        self#task_snippet ~sp
          ~width ~depth ~padding ~char_size ~message:t.Types.t_message
                                                               >>= fun snip ->
        let snip = Ocamlduce.Utf8.make snip in
        Roles.get_area_role sp t.Types.t_area                  >>= fun role ->
        !!(role.Roles.task_property_editor)                    >>= fun editor ->
        Data.get_kinds ~sp ~area:t.Types.t_area                >>= fun alt_k ->

          Lwt.return
            {{ <tr>[
                 <th align="left">[
                   <div class={:"depth" ^ (string_of_int depth):}>[
                      !snip
                   ]
                 ]
                 !{: 
                     List.map
                      (fun field ->
                         if editor
                         then
                           let editable_field =
                             self#show_editable_field
                                ~sp ~task:t alt_k field
                           in ({{ <td>[ editable_field ] }}
                                 : {{ Xhtmltypes_duce.td }})
                         else
                           ((self#show_static_field ~field ~task:t )
                              : {{ Xhtmltypes_duce.td }})
                      ) 
                      fields
                 :}
               ]

            }}
      in
      let rec show_tree ~width ~depth ~tree:t = match t with
        | Types.Tree.Nil ->
            let err = Ocamlduce.Utf8.make "Undefined task id." in
            Lwt.return ({{ [ <tr>[ <td>err ] ] }} : {{ [Xhtmltypes_duce.tr] }} )
        | Types.Tree.Node (t,l) ->
            (show_line ~width ~depth ~task:t
               >>= fun a ->
             Lwt_util.map_serial
               (fun tree -> show_tree ~width ~depth:(succ depth) ~tree)
               l
               >>= fun b ->
             let b = List.fold_left
                       (fun
                          (e1 : {{ [ Xhtmltypes_duce.tr* ] }})
                          (e2 : {{ [ Xhtmltypes_duce.tr+ ] }})
                          -> {{ [ !e1 !e2 ] }}
                       )
                       ({{ [ ] }} : {{ [ Xhtmltypes_duce.tr* ] }})
                       b
             in
             Lwt.return ({{ [ a !{: b :} ] }} : {{ [ Xhtmltypes_duce.tr+ ] }}))
      in
        show_tree ~width ~depth:0 ~tree
          >>= fun core ->
        let core : {{ [ Xhtmltypes_duce.tr+ ] }} = core in
        let head = self#header ~fields in
          Lwt.return
            (({{ <table
                     cellpadding = "15px"
                     border      = "2px"
                     width       = "90%"
                     rules       = "cols" >[
                   !{: head :}
                   !{: core :}
                 ]
              }} : {{ Xhtmltypes_duce.table }} ) : {{ Xhtmltypes_duce.block }})

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
