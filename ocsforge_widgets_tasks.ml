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
    | None -> failwith "FIXME: manage error better"
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


(*
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
*)


(* is to be included in a <noscript>... /!\ Not pretty to look at /!\ *)
class tree_widget =
object (self)

  val task_text_class = "ocsforge_task_text"

  method private header ~fields =
    ({{ [ <thead>[
            <tr>[
              <th>[ ' tasks' ]
              <th>[ ' repository ' ]
              !{: List.map
                      (fun s -> {{ <td>[ !{: to_utf8 s :} ] }} )
                      fields
               :}
        ] ] ] }} : {{ [ Xhtmltypes_duce.thead ] }} )

  method private task_snippet ~sp ~message inline_widget =
    draw_message_title ~sp ~message inline_widget

  method private show_static_field ~field ~task:t =
    {{ <td>
        {: let f = Olang.string_of_t_opt in
              (match field with
                 | "progress" ->
                     f Int32.to_string t.Types.t_progress
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


  method private display_noscript ~sp ~root_task inline_widget =
    let fields =
      ["length" ; "importance" ; "deadline_version" ; "deadline_time" ; "kind" ]
    in
    Data.get_tree ~sp ~root:root_task () >>= fun tree ->
      let show_line ~task:t =
        self#task_snippet ~sp ~message:t.Types.t_message inline_widget
                                                               >>= fun snip ->
        (match (Services_ht.find_service t.Types.t_id) with
          | None -> Lwt.return ({{ [ ] }} : {{ [Xhtmltypes_duce.a*] }})
          | Some s -> Lwt.return
             {{ [ {: EDuce.Xhtml.a
                        ~service:s.Services_ht.sources_service
                        ~sp
                        {{ [ 'repository' ] }}
                        ([],(None,(false,(None,(false,(false,(None,None)))))))
             :} ] }} )
                                                         >>= fun repo_link ->
        Lwt_util.map_serial
          (fun field -> Lwt.return ( self#show_static_field ~field ~task:t ) )
          fields
                                                          >>= fun fields ->
        Lwt.return
            {{ <tr>[
                 <th align="left">[ snip ]
                 <td>[ !repo_link ]
                 !{: fields :}
            ] }}
      in
      let rec show_tree = fun
         { Types.Tree.content = t ; Types.Tree.children = l } ->
            (show_line ~task:t
               >>= fun a ->
             Lwt_util.map_serial
               (fun tree -> show_tree tree)
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
        show_tree tree         >>= fun core ->
        let head = self#header ~fields in
          Lwt.return
            ( ( {{ <table>[ !head !core ] }} ) : {{ Xhtmltypes_duce.table }})

  method display ~sp ~root_task inline_widget =
    ( self#display_noscript ~sp ~root_task inline_widget ) >>= fun ns ->
    (*TODO: add an add_onload call*)
    Lwt.return
      ({{ [ <div id="ocsforge_task_tree">[ <noscript>[ ns ] ] ] }}
         : {{ Xhtmltypes_duce.flows }} )

end

