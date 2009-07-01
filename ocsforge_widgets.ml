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
module Services = Ocsforge_services
module Params = Eliom_parameters
module EDuce = Eliom_duce
module Olang = Ocsforge_lang

open CalendarLib

let to_utf8 = Ocamlduce.Utf8.make

(** draw a select field for optional values.
* If a list of alternative is provided, it makes it selectable.*)
let draw_opt_field ~name ?alternatives ~string_of_t () =
  {{ [ {: match alternatives with
    | None | Some []->
          EDuce.Xhtml.user_type_select
             (Olang.string_of_t_opt string_of_t)
             ~name
             (EDuce.Xhtml.Option ({{ { } }}, None, None, true))
             []
    | Some l ->
          EDuce.Xhtml.user_type_select
             (Olang.string_of_t_opt string_of_t)
             ~name
             (EDuce.Xhtml.Option ({{ { } }}, None, None, true)) 
             (List.map
                (fun a ->
                  EDuce.Xhtml.Option ({{ { } }}, Some a, None, false))
                l)
  :} ] }}

let draw_field ~name ~value ?alternatives ~string_of_t () =
  match (value,alternatives) with
    | (v, None)   -> {{ {: to_utf8 (string_of_t v) :} }}
    | (v, Some l) ->
        {{ [ {:
          EDuce.Xhtml.user_type_select
             string_of_t
             ~name
             (EDuce.Xhtml.Option ({{ { } }}, v, None, true))
             (List.map
                (fun a ->
                   EDuce.Xhtml.Option ({{ { } }}, a, None, false))
                l)
        :} ] }}

let draw_savable_field ~sp ~service ~id ~value ~string_of_t ~alts () =
  EDuce.Xhtml.post_form
    ~service ~sp
    (fun (id_name, param_name) ->
       match (value,alts) with
         | (None,   []) ->
             {{ [ <p>{: to_utf8 "none" :}  ] }}
         | (Some v, [])->
             {{ [ <p>{: to_utf8 (string_of_t v) :} ] }}
         | (None,   l) ->
             {{ [ <p>[
                 {: EDuce.Xhtml.user_type_select
                      (Olang.string_of_t_opt string_of_t)
                      ~name:param_name
                      (EDuce.Xhtml.Option ({{ { } }}, None, None, true)) 
                      (List.map
                         (fun a -> EDuce.Xhtml.Option
                                     ({{ { } }}, Some a, None, false)
                         )
                         l)
                 :}
                 {: EDuce.Xhtml.user_type_button
                      ~name:id_name
                      ~value:id
                      Types.string_of_task
                      {{ "Save" }}
                 :}
             ] ] }}
         | (Some v, l) ->
             {{ [ <p>[
                 {:EDuce.Xhtml.user_type_select
                     (Olang.string_of_t_opt string_of_t)
                     ~name:param_name
                     (EDuce.Xhtml.Option ({{ { } }}, Some v, None, true))
                     ( (List.map
                         (fun a -> EDuce.Xhtml.Option
                                    ({{ { } }}, Some a, None, false))
                         l)
                      @[EDuce.Xhtml.Option ({{ { } }}, None, None, false)])

                 :}
                 {: EDuce.Xhtml.user_type_button
                      ~name:id_name
                      ~value:id
                      Types.string_of_task
                      {{ "Save" }}
                 :}
             ] ] }}
    )
    ()


(*most of the time, the obrowser script will be used !*)
class new_task_widget =
object

  val add_task_class = "ocsforge_add_task_form"

  method display ~sp ~parent ?(rows = 6) ?(cols = 50) () =
    Data.get_task ~sp ~task:parent               >>= fun p_info ->
    Data.get_area ~sp ~area:p_info.Types.t_area  >>= fun p_a_info ->
    Data.get_kinds ~sp ~area:p_a_info.Types.r_id >>= fun alt_k ->
    Forum_data.get_message ~sp ~message_id:p_info.Types.t_message
                                                 >>= fun p_msg ->

    let draw_form
          ((parent_name,
            (subject_name,
             (text_name,
              (length_name,
               (progress_name,
                (importance_name,
                 (deadline_t_name,
                  (deadline_v_name,
                   (kind_name,
                    detach_name)))))))))
           : ([ `One of Types.task ] Params.param_name *
              ([ `One of string ] Params.param_name *
               ([ `One of string ] Params.param_name *
                ([ `One of CalendarLib.Calendar.Period.t option ]
                                 Params.param_name *
                 ([ `One of int32 option ] Params.param_name *
                  ([ `One of int32 option ] Params.param_name *
                   ([ `One of CalendarLib.Date.t option ]
                                    Params.param_name *
                    ([ `One of string ] Params.param_name *
                     ([ `One of string option ] Params.param_name *
                      [ `One of bool ] Params.param_name))))))))
           )) =
      {{
        [<p>[
           'Creating sub task for : '
           !{: to_utf8
                 (Olang.unopt
                    ~default:"undescribed task"
                    p_msg.Forum_sql.Types.m_subject)
           :}
           <br>[]
           {: EDuce.Xhtml.string_input
                ~a:{{ { size="40%" } }}
                ~input_type:{: "text" :}
                ~name:subject_name () :}
           <br>[]
           {: EDuce.Xhtml.textarea ~name:text_name ~rows ~cols () :}
         ]
        <p>[
          'length : '
          !{: draw_opt_field
               ~name:length_name
               ~alternatives:Types.Alts.lengths 
               ~string_of_t:Olang.string_of_period
               () :}
          <br>[]
          'progress : '
          !{: draw_opt_field
                ~name:progress_name
                ~string_of_t:Int32.to_string
                ~alternatives:Types.Alts.percents
                () :}
          <br>[]
          'importance : '
          !{: draw_opt_field
               ~name:importance_name
               ~alternatives:Types.Alts.percents
               ~string_of_t:Int32.to_string
               () :}
          <br>[]
          'deadline (time) : '
          !{: draw_opt_field
                ~name:deadline_t_name
                ~alternatives:Types.Alts.deadlines
                ~string_of_t:Olang.string_of_date
                () :}
          <br>[]
          'deadline (version) : '
          {: EDuce.Xhtml.string_input
               ~a:{{ { size="9%" } }}
               ~name:deadline_v_name
               ~input_type:{: "text" :}
               () :}
          <br>[]
          'kind : '
          !{: draw_opt_field
                ~name:kind_name
                ~alternatives:alt_k
                ~string_of_t:(fun k -> k)
                () :}
          <br>[]
          'detach : '
          {: EDuce.Xhtml.bool_checkbox
               ~name:detach_name () :}
        ]
        <p>[
          {: EDuce.Xhtml.user_type_button
                ~name:parent_name
                ~value:p_info.Types.t_id
                Types.string_of_task
                {{ "Save" }}
          :}

        
        ]
       ]
     }}
    in
      Lwt.return
        (EDuce.Xhtml.post_form
           ~service:Services.new_task_service
           ~sp draw_form ())


end



class tree_widget =
  let fresh_id_well = ref 0 in
object (self)

  val task_text_class = "ocsforge_task_text"

  method fresh_id = incr fresh_id_well ; !fresh_id_well

  method private header ~fields ~sp ~id ?parent ~nl_service =
    let field_count = List.length fields in
    ({{ [ <colgroup>[
           <col>[] (*the task column*)
           !{: List.map (fun _ -> {{ <col width={: (string_of_int
                                                    (min (60 / field_count) 12))
                                                   ^ "%" :}>[]
                                  }} )
            fields
           (*There's no division by zero problem because the function is
            * called only when the list isn't empty*)
          :}
       ]
       <thead>[
         <tr>[
           <th>[ {: match parent with
                     | None -> {{ ' ' }}
                     | Some parent ->
                         EDuce.Xhtml.a
                           ~service:nl_service
                           ~sp {{ "UP" }} ( (), (parent, ("", false)))
                 :}
                 ' tasks'
           ]
           !{: List.map
                (function s ->
                  {{ <td>[ !{: match s with
                       | "importance" ->
                            {{ [ 'importance'
                                 <a id="importance_hl_script"
                                    class="jslink"
                                    onclick={: (  "caml_run_from_table"
                                                ^ "(main_vm, 289, "
                                                ^ (Eliom_obrowser.jsmarshal
                                                     "importance")
                                                ^ ")" ):}>[
                                      <img src={: EDuce.Xhtml.make_uri ~sp
                                                    ~service:(
                                                    Eliom_services.static_dir
                                                    ~sp)
                                                    ["highlighter.png"] :}
                                           alt="highligth">[]
                                  ]
                            ] }}
                       | "deadline_time" ->
                           {{ [ 'deadline'
                                <a id="deadline_hl_script"
                                   class="jslink"
                                   onclick={: (  "caml_run_from_table"
                                               ^ "(main_vm, 289, "
                                               ^ (Eliom_obrowser.jsmarshal
                                                    "deadline")
                                               ^ ")" ):}>[
                                     <img src={: EDuce.Xhtml.make_uri ~sp
                                                   ~service:(
                                                   Eliom_services.static_dir
                                                   ~sp)
                                                   ["highlighter.png"] :}
                                          alt="highligth">[]
                                ]
                           ] }}
                       | "progress" ->
                           {{ [ 'progress'
                                <a id="progress_hl_script"
                                   class="jslink"
                                   onclick={: (  "caml_run_from_table"
                                               ^ "(main_vm, 289, "
                                               ^ (Eliom_obrowser.jsmarshal
                                                    "complete")
                                               ^ ")" ):}>[
                                     <img src={: EDuce.Xhtml.make_uri ~sp
                                                   ~service:(
                                                   Eliom_services.static_dir
                                                   ~sp)
                                                   ["highlighter.png"] :}
                                            alt="highligth">[]
                                ]
                           ] }}
                       | _ -> to_utf8 s
                       :}

                       {: EDuce.Xhtml.a
                            ~service:nl_service
                            ~sp
                            {{ [ <img src={: EDuce.Xhtml.make_uri ~sp
                                                ~service:(
                                                Eliom_services.static_dir ~sp)
                                                ["up.png"] :}
                                      alt="sort">[] ] }}
                            ( (), (id, (s, false)))
                       :}

                       {: EDuce.Xhtml.a
                            ~service:nl_service
                            ~sp 
                            {{ [ <img src={: EDuce.Xhtml.make_uri ~sp
                                                ~service:(
                                                Eliom_services.static_dir ~sp)
                                                ["down.png"] :}
                                      alt="sort">[] ] }}
                            ( (), (id, (s, true)))
                       :}
                  ] }}
               )
               fields
           :}
         ] ] ] }} : {{ [ Xhtmltypes_duce.colgroup Xhtmltypes_duce.thead ] }} )

  method private task_snippet ~sp ~width ~depth ~padding ~char_size ~message =
    Forum_data.get_message ~sp ~message_id:message >>= fun msg ->
    let shorten str dep wid pad siz =
      let max_size = (wid - (dep * pad)) / siz in
      if String.length str > max_size
      then (String.sub str 0 max_size) ^ "..."
      else str
    in
    let res =
      shorten
        (Olang.unopt ~default:"\"NONE\"" msg.Forum_sql.Types.m_subject)
        depth width padding char_size
    in

    Lwt.return res

  method private show_static_field ~field ~task:t =
    {{ <td>{:
      match field with
    | "progress" ->
        Olang.string_of_t_opt Int32.to_string        t.Types.t_progress
    | "importance" ->
        Olang.string_of_t_opt Int32.to_string        t.Types.t_importance
    | "kind" ->
        Olang.string_of_t_opt (fun k -> k)           t.Types.t_kind
    | "deadline_time" ->
        Olang.string_of_t_opt Olang.string_of_date   t.Types.t_deadline_time
    | "deadline_version" ->
        Olang.string_of_t_opt (fun d -> d)           t.Types.t_deadline_version
    | "length" ->
        Olang.string_of_t_opt Olang.string_of_period t.Types.t_length
    | _ -> ""
    :} }}

  method private show_editable_field ~sp ~td_id ~task:t alt_k = function
    | "progress" ->
        {{ <td id={: td_id :}>[ {:
        draw_savable_field ~sp ~service:Services.set_progress_service
          ~id:t.Types.t_id     ~value:t.Types.t_progress
          ~string_of_t:Int32.to_string ~alts:Types.Alts.percents       ()
        :} ] }}
    | "importance" ->
        {{ <td id={: td_id :}
               onclick={: "caml_run_from_table(main_vm, 489, "
                          ^(Eliom_obrowser.jsmarshal
                            (td_id, t.Types.t_importance, t.Types.t_id))
                          ^")" :}>[
           <div>{: (Olang.string_of_t_opt ~quote:"" Int32.to_string)
                      t.Types.t_importance :}
        ] }}
    | "kind" ->
        {{ <td id={: td_id :}>[ {:
        draw_savable_field ~sp ~service:Services.set_kind_service
          ~id:t.Types.t_id     ~value:t.Types.t_kind
          ~alts:alt_k          ~string_of_t:(fun s -> s)               ()
        :} ] }}
    | "deadline_time" ->
        {{ <td id={: td_id :}>[ {:
        draw_savable_field ~sp ~service:Services.set_deadline_time_service
          ~id:t.Types.t_id     ~value:t.Types.t_deadline_time
          ~alts:Types.Alts.deadlines ~string_of_t:Olang.string_of_date ()
        :} ] }}
    | "deadline_version" ->
        {{ <td id={: td_id :}
               onclick={: "caml_run_from_table(main_vm, 389, "
                          ^(Eliom_obrowser.jsmarshal
                            (td_id,
                             Olang.unopt ~default:"" t.Types.t_deadline_version,
                             t.Types.t_id))
                          ^")" :}>[
           <div>{: Olang.unopt ~default:"" t.Types.t_deadline_version :}
        ] }}
    | "length" ->
        {{ <td id={: td_id :}>[ {:
        draw_savable_field ~sp ~service:Services.set_length_service
          ~id:t.Types.t_id      ~value:t.Types.t_length
          ~alts:Types.Alts.lengths ~string_of_t:Olang.string_of_period ()
        :} ] }}
    | _ -> {{ <td>[<p>[ 'unknwown' ]] }}


  method display ~sp ~root_tasks:(root_task, top_root) ~fields
        ~(nl_service :
            (unit * (Types.task * (string * bool)), unit,
             [ `Nonattached of 'a Eliom_services.na_s ], [ `WithoutSuffix ],
             unit *
             ([ `One of Types.task ] Params.param_name *
              ([ `One of string ] Params.param_name *
               [ `One of bool ] Params.param_name )),
             unit, [ `Unregistrable ])
            Eliom_services.service)
       ~(repo_service :
	   (string * string list, unit,
            [ `Attached of
              Eliom_services.get_attached_service_kind Eliom_services.a_s ],
            [ `WithSuffix ],
            [ `One of string ] Eliom_parameters.param_name *
            [ `One of string list ] Eliom_parameters.param_name, unit,
            [ `Registrable ])
           Eliom_services.service
	)
      ?(width = 600) ?(padding = 10) ?(char_size = 12)
      ?sort
      () =
    Data.get_tree ~sp ~root:root_task >>= fun tree ->
      let show_line ~width ~depth ~task:t =
        self#task_snippet ~sp
          ~width ~depth ~padding ~char_size ~message:t.Types.t_message
                                                               >>= fun snip ->
        let snip = to_utf8 snip in
        Roles.get_area_role sp t.Types.t_area                  >>= fun role ->
        !!(role.Roles.task_property_editor)                    >>= fun editor ->
        Data.get_kinds ~sp ~area:t.Types.t_area                >>= fun alt_k ->
        let classes =
          let ten = Int32.of_int 10 in
            ("depth" ^ (string_of_int (min depth 9)))
          ::("importance" ^ (Int32.to_string
                               (Int32.div
                                  (Olang.unopt ~default:Int32.zero
                                     t.Types.t_importance)
                                  ten)))
          ::("deadline" ^ (string_of_int
                             (Olang.unopt ~default:0
                             (Olang.apply_on_opted
                               Olang.urgency
                                t.Types.t_deadline_time))))
          ::("complete" ^ (Int32.to_string
                             (Int32.div
                                (Olang.unopt ~default:Int32.zero
                                   t.Types.t_progress)
                                ten)))
          ::[]
        in

          Lwt.return
            {{ <tr class={: Ocsimore_lib.build_class_attr classes :} style="">[
                 <th align="left">[
                    <div class={: Ocsimore_lib.build_class_attr
                                    ["depth" ^ (string_of_int (min depth 9))]
                       :}>[
		       {: EDuce.Xhtml.a
                          ~service:repo_service
                          ~sp
                          {{ [ <img src={: EDuce.Xhtml.make_uri ~sp
                                            ~service:(Eliom_services.static_dir
                                                         ~sp)
                                            ["open_repository.png"] :}
                                   alt="go to repos page">[]
                             ] }}
                          (Types.string_of_task t.Types.t_id, [""])
                     :} 
                     {: EDuce.Xhtml.a
                          ~service:nl_service
                          ~sp
                          {{ [ <img src={: EDuce.Xhtml.make_uri ~sp
                                            ~service:(Eliom_services.static_dir
                                                         ~sp)
                                            ["magnifier.png"] :}
                                   alt="zoom to subtask">[]
                             ] }}
                          ( (), (t.Types.t_id, ("", false)))
                     :} 
                     <a class="jslink"
                         onclick={: (  "caml_run_from_table (main_vm, 189, "
                                     ^ (Eliom_obrowser.jsmarshal t.Types.t_id)
                                     ^ ")") :}>[
                         <img alt="add subtask"
                              src={: EDuce.Xhtml.make_uri ~sp
                                        ~service:(Eliom_services.static_dir ~sp)
                                        ["add.png"] :}>[]]
                     !snip
                   ]
                 ]
                 !{: 
                     List.map
                       (fun field ->
                          if editor
                          then
                            let td_id =
                              "td_id_" ^ (string_of_int self#fresh_id)
                            in
                            let editable_field =
                              self#show_editable_field
                                 ~sp ~td_id ~task:t alt_k field
                            in ( editable_field : {{ Xhtmltypes_duce.td }})
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
        | Types.Tree.Nil -> (*TODO: raise a Ocsimore_common.Permission_denied*)
            let err = to_utf8 "Undefined task id." in
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
      let tree = match sort with
        | None      -> tree
        | Some comp -> Types.Tree.sort ~tree ~comp
      in
        show_tree ~width ~depth:0 ~tree
          >>= fun core ->
        let core : {{ [ Xhtmltypes_duce.tr+ ] }} = core in
        let head =
          let parent =
            if root_task = top_root
            then None
            else Some (match tree with
                        | Types.Tree.Nil -> root_task
                        | Types.Tree.Node (t, _) -> t.Types.t_parent)
          in
            self#header
              ~fields ~sp ~id:root_task ?parent ~nl_service
        in
          Lwt.return
            (({{ [
                  <table
                     cellpadding = "0px"
                     border      = "2px"
                     width       = "90%"
                     rules       = "cols"
                     id          = "ocsforge_tree" >[
                   !{: head :}
                   !{: core :}
                 ] ]
              }} ) : {{ [ Xhtmltypes_duce.block ] }})

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
               ~alternatives:(Olang.int_interval_list
                                ~bump:5 ~min:0 ~max:100)
               ~string_of_t:Int32.to_string ()
          :}
        <br>[]
          {: draw_opt_field
               ~title:"importance" ~name:importance_name
               ~value:info.Types.t_importance
               ~alternatives:(
                 lazy (Olang.int_interval_list ~bump:5 ~min:0 ~max:100))
               ~string_of_t:Int32.to_string ()
          :}
        <br>[]
          {: draw_opt_field
               ~title:"deadline (time)" ~name:deadline_t_name
               ~value:info.Types.t_deadline_time
               ~alternatives:((Olang.date_interval_list ~min:1 ~max:7)
                             @(Olang.date_interval_list
                                ~bump:(Calendar.Period.lmake
                                         ~day:7 ())
                                ~min:1 ~max:(7 * 4)))
               ~string_of_t:Printer.Calendar.to_string ()
          :}
        <br>[]
          {: {{ {: "deadline (version) : " :}
                {: (EDuce.Xhtml.string_input
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
                 ( (Olang.period_interval_list ~min:1 ~max:6)
                  @(Olang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:12 ())
                      ~min:12 ~max:24)
                  @(Olang.period_interval_list
                      ~bump:(Calendar.Period.lmake ~hour:24 ())
                      ~min:48 ~max:96))))
               ~string_of_t:(Olang.string_of_period) ()
          :}
        <br>[]
          {: {{ {: "Detach : " :}
                {: EDuce.Xhtml.bool_checkbox ~name:detach_name () :}
             }}
          :}
        ]
      }}
      in EDuce.Xhtml.get_form
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
             {:"deadline (version) : " :} {: info.Roles.t_deadline_version :}
               <br>[]
             {:"length : " :}             {: info.Roles.t_length :}
               <br>[]
        ]
        }}

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
