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
module Services = Ocsforge_services_tasks
module Services_ht = Ocsforge_services_hashtable
module Params = Eliom_parameters
module EDuce = Eliom_duce
module Olang = Ocsforge_lang
module FTypes = Forum_sql.Types

open CalendarLib

let to_utf8 = Ocamlduce.Utf8.make
let run i arg = "caml_run_from_table (main_vm,"
              ^ (string_of_int i) ^ ","
              ^ (Eliom_obrowser.jsmarshal arg) ^ ")"

let draw_message_title ~sp ~message
      (inline_widget : Wiki_widgets_interface.frozen_wikibox) =
  Forum_data.get_message ~sp ~message_id:message >>= fun minfo ->
  match minfo.FTypes.m_subject with
    | None -> Lwt.return {{ <span>['Undescribed task'] }}
    | Some s -> (*TODO: get rid of Wiki_sql uses *)
        Wiki_sql.wikibox_wiki minfo.FTypes.m_wikibox  >>= fun wiki ->
        Wiki_sql.get_wiki_info_by_id wiki      >>= fun winfo ->
        let rights = Wiki_models.get_rights winfo.Wiki_types.wiki_model in
        Wiki.default_bi ~sp ~wikibox:s ~rights >>= fun bi ->
        inline_widget#display_frozen_wikibox ~bi ~classes:[] ~wikibox:s
          >>= function (*TODO: don't use this exception prone match *)
                | {{ <div class=_>[ <h1>[ <span>[ (x::_)* ]]] }} -> Lwt.return {{ <span>[ !x ] }}
                | {{ a }}            -> (Ocamlduce.Print.print_xml print_endline a ; failwith "Not an h1 message title")


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
                (fun a -> EDuce.Xhtml.Option ({{ { } }}, Some a, None, false))
                l
             )
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
                (fun a -> EDuce.Xhtml.Option ({{ { } }}, a, None, false))
                l
             )
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

let visual_percent ~color ~bg_color ~value () =
  match value with
    | Some v -> let value = min 100 (max 0 v) in
        {{
           <table width={: "100%" :} rules="none" border={: "0px" :}>[
             <colgroup>[
               <col width={: (string_of_int value) ^ "%" :}>[]
               <col width={: (string_of_int (100 -value)) ^ "%" :}>[]
             ]
             <tr>[
               <td style={: "background-color:" ^ ( color value ) :}
                   align="right">[
                 !{: to_utf8
                       (if value > 50
                        then (string_of_int value) ^ "%"
                        else "") :}
               ]
               <td style={: "background-color:" ^ ( bg_color value ) :}
                   align="left">[
                 !{: to_utf8
                       (if value <= 50
                        then (string_of_int value) ^ "%"
                        else "") :}
               ]
             ]
           ]
        }}
    | None ->
        {{ <table width={: "100%" :}>[
              <col width="100%">[]
              <tr>[
                <td style={: "color:" ^ ( bg_color 0 ) :}>[
                'None'
                ]
              ]
           ]
        }}





(*most of the time, the obrowser script will be used !*)
class new_project_widget =
object

  val add_task_class = "ocsforge_add_task_form"

  method display ~sp ~parent
                 (inline_widget : Wiki_widgets_interface.frozen_wikibox) =
    Data.get_task ~sp ~task:parent               >>= fun p_info ->
    Data.get_area ~sp ~area:p_info.Types.t_area  >>= fun p_a_info ->
    Data.get_kinds ~sp ~area:p_a_info.Types.r_id >>= fun alt_k ->
(*    Forum_data.get_message ~sp ~message_id:p_info.Types.t_message
                                                 >>= fun p_msg ->*)

    let draw_form
          inline_widget
          ((parent_name,
            (subject_name,
             (text_name,
              (length_name,
               (progress_name,
                (importance_name,
                 (deadline_t_name,
                  (deadline_v_name,
                   kind_name))))))))
           : ([ `One of Types.task ] Params.param_name *
              ([ `One of string ] Params.param_name *
               ([ `One of string ] Params.param_name *
                ([ `One of CalendarLib.Calendar.Period.t option ]
                                 Params.param_name *
                 ([ `One of int32 option ] Params.param_name *
                  ([ `One of int32 option ] Params.param_name *
                   ([ `One of CalendarLib.Date.t option ]
                                    Params.param_name *
                    ([ `One of string option ] Params.param_name *
                     [ `One of string option ] Params.param_name)))))))
           )) =
      draw_message_title
        ~sp ~message:p_info.Types.t_message inline_widget >>= fun title ->
      Lwt.return
      {{
        [<p>[
           'Creating sub task for ' title
           <br>[]
           {: EDuce.Xhtml.string_input
                ~a:{{ { size="40%" } }}
                ~input_type:{: "text" :}
                ~name:subject_name () :}
           <br>[]
           {: EDuce.Xhtml.textarea ~name:text_name ~rows:1 ~cols:60 () :}
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
          'deadline (version) : '              (*TODO:use a string_input field*)
          !{: draw_opt_field
                ~name:deadline_v_name
                ~alternatives:[]
                ~string_of_t:(fun k -> k)
                () :}
          <br>[]
          'kind : '
          !{: draw_opt_field
                ~name:kind_name
                ~alternatives:alt_k
                ~string_of_t:(fun k -> k)
                () :}
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
        (EDuce.Xhtml.lwt_post_form
           ~service:Services.new_task_service
           ~sp (draw_form inline_widget) ())


end



class tree_widget =
  let fresh_id_well = ref 0 in
object (self)

  val task_text_class = "ocsforge_task_text"

  method fresh_id = incr fresh_id_well ; !fresh_id_well

  method private header ~fields ~sp ~id =
    let field_count = List.length fields in
    ({{ [ <colgroup>[
            <col>[ ] (*the task column*)
            !{: List.map
                 (fun _ ->
                    {{ <col width={: (string_of_int (min (60 / field_count) 12))
                                    ^ "%"
                                  :} >[ ]
                    }} )
                 fields
           (*There's no division by zero problem because the function is
            * called only when the list isn't empty*)
            :}
          ]
          <thead>[
            <tr>[
              <th>[ ' tasks' ]
              !{: List.map
                   (fun s -> {{ <td>[ !{: match s with
                     | "importance" ->
                         {{ [ 'importance'
                              <a id="importance_hl_script"
                                 class="jslink"
                                 onclick={: run 289 "importance" :}>[
                                <img src={: EDuce.Xhtml.make_uri
                                              ~sp
                                              ~service:(
                                                 Eliom_services.static_dir
                                                 ~sp
                                              )
                                              ["highlighter.png"]
                                         :}
                                     alt="highligth">[]
                              ]
                         ] }}
                     | "deadline_time" ->
                         {{ [ 'deadline'
                              <a id="deadline_hl_script"
                                 class="jslink"
                                 onclick={: run 289 "deadline" :}>[
                                <img src={: EDuce.Xhtml.make_uri
                                              ~sp
                                              ~service:(
                                                 Eliom_services.static_dir
                                                 ~sp
                                              )
                                              ["highlighter.png"]
                                         :}
                                     alt="highligth">[]
                                ]
                         ] }}
                     | "progress" ->
                         {{ [ 'progress'
                              <a id="progress_hl_script"
                                 class="jslink"
                                 onclick={: run 289 "complete" :}>[
                                <img src={: EDuce.Xhtml.make_uri
                                              ~sp
                                              ~service:(
                                                 Eliom_services.static_dir
                                                 ~sp
                                              )
                                              ["highlighter.png"]
                                         :}
                                     alt="highligth">[]
                              ]
                         ] }}
                     | "deadline_version" -> to_utf8 "deadline"
                     | _ -> to_utf8 s
                   :} ] }} )
                   fields
               :}
         ] ] ] }} : {{ [ Xhtmltypes_duce.colgroup Xhtmltypes_duce.thead ] }} )

  method private task_snippet ~sp ~message inline_widget =
    draw_message_title ~sp ~message inline_widget
    


  method private show_static_field ~field ~task:t =
    match field with
      | "progress" ->
          {{ <td>
              [ {: visual_percent
                     ~color:(
                        fun v ->   "rgb(100,"
                                 ^ (string_of_int (100 + v / 2)) ^ ","
                                 ^ "255)"
                     ) 
                     ~bg_color:(fun _ -> "rgb(240,240,240)")
                     ~value:(
                       Olang.apply_on_opted Int32.to_int t.Types.t_progress
                     )
                     ()
              :} ]
          }}
      | field ->
         {{ <td>
             {: let f = Olang.string_of_t_opt in
                   (match field with
                      | "importance" ->
                          f Int32.to_string t.Types.t_importance
                      | "kind" ->
                          f (fun k -> k) t.Types.t_kind
                      | "deadline_time" ->
                          f Olang.string_of_date t.Types.t_deadline_time
                      | "deadline_version" ->
                          f (fun d -> d) t.Types.t_deadline_version
                      | "length" ->
                          f Olang.string_of_period t.Types.t_length
                      | _ -> f (fun _ -> "") None)
             :}
         }}

  method private show_editable_field ~sp ~td_id ~task:t alt_k = function
    | "progress" ->
        {{ <td id={: td_id :}>[ {:
        visual_percent
          ~color:(
            fun v -> "rgb(100,"
                        ^ (string_of_int (100 + v / 2)) ^ ","
                        ^ "255)"
          )
          ~bg_color:(fun _ -> "rgb(240,240,240)")
          ~value:(Olang.apply_on_opted Int32.to_int t.Types.t_progress)
          ()
        :} ] }}
    | "importance" ->
        {{ <td id={: td_id :}
               onclick={: "caml_run_from_table(main_vm, 489, "
                          ^(Eliom_obrowser.jsmarshal
                            (td_id,
                             "importance",
                             t.Types.t_importance,
                             t.Types.t_id))
                          ^")" :}>[
           <div>{: (Olang.string_of_t_opt Int32.to_string)
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
                             "deadline_v",
                             Olang.unopt ~default:"None" t.Types.t_deadline_version,
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


  method display ~sp ~root_task ~fields inline_widget =
    Data.get_tree ~sp ~root:root_task () >>= fun tree ->
      let show_line ~depth ~task:t =
        self#task_snippet ~sp ~message:t.Types.t_message inline_widget
                                                               >>= fun snip ->
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
        (match (Services_ht.find_service t.Types.t_id) with
          | None -> Lwt.return ({{ [ ] }} : {{ [Xhtmltypes_duce.a*] }})
          | Some s -> Lwt.return
             {{ [ {:
               EDuce.Xhtml.a ~service:s.Services_ht.sources_service ~sp
                 {{ [ <img src={: EDuce.Xhtml.make_uri
                                    ~sp
                                    ~service:(Eliom_services.static_dir ~sp)
                                    ["open_repository.png"]
                               :}
                           alt="Go to repository page"> [ ]
                 ] }}
                 ([],(None,(false,(None,(false,(false,(None,None)))))))
             :} ] }} )                          >>= fun repo_link ->
        Lwt_util.map_serial
          (fun field ->
             Lwt.return (
               if editor
               then
                 self#show_editable_field
                   ~sp
                   ~td_id:( "td_id_" ^ (string_of_int self#fresh_id) )
                   ~task:t
                   alt_k
                   field
               else self#show_static_field ~field ~task:t
             )
          )
          fields  >>= fun fields ->
        Lwt.return
            {{ <tr class={: Ocsimore_lib.build_class_attr classes :} style="">
                 [ <th align="left">
                     [ <div class={: Ocsimore_lib.build_class_attr
                                       ["depth" ^ (string_of_int (min depth 9))]
                                  :}>
                         [ !repo_link
                           <a class="jslink"
                              onclick={: run 189 t.Types.t_id :}>
                             [ <img alt="add subtask"
                                    src={: EDuce.Xhtml.make_uri
                                             ~sp
                                             ~service:(
                                               Eliom_services.static_dir ~sp
                                             )
                                             ["add.png"] :}>
                                 [ ]
                             ]
                           snip
                         ]
                     ] !{: fields :}
                 ]
            }}
      in
      let rec show_tree ~depth = fun
         { Types.Tree.content = t ; Types.Tree.children = l } ->
            (show_line ~depth ~task:t
               >>= fun a ->
             Lwt_util.map_serial
               (fun tree -> show_tree ~depth:(succ depth) tree)
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
             Lwt.return ({{ [ a !b ] }} : {{ [ Xhtmltypes_duce.tr+ ] }}))
      in
        show_tree ~depth:0 tree         >>= fun core ->
(*TYPE*)    let core : {{ [ Xhtmltypes_duce.tr+ ] }} = core in
        let head = self#header ~fields ~sp ~id:root_task in
(*TYPE*)    let head : {{ [ Xhtmltypes_duce.colgroup Xhtmltypes_duce.thead ] }} = head in
          Lwt.return
            (({{ [
                  <table cellpadding = "0px"
                         border      = "2px"
                         width       = "90%"
                         rules       = "cols"
                         id          = "ocsforge_tree" >
                    [ !head !core ]
                 ] }} ) : {{ [ Xhtmltypes_duce.table ] }})

end

