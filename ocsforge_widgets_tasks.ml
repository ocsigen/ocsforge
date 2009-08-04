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
module FTypes = Forum_types

open CalendarLib

let to_utf8 = Ocamlduce.Utf8.make
let run i arg = "caml_run_from_table (main_vm,"
              ^ (string_of_int i) ^ ","
              ^ (Eliom_obrowser.jsmarshal arg) ^ ")"

let draw_message_title ~sp ~task = Data.find_subject ~sp ~task

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
  match alternatives with
    | None      -> {{ {: to_utf8 ( string_of_t value ) :} }}
    | Some alts ->
        {{ [ {:
          EDuce.Xhtml.user_type_select
             string_of_t
             ~name
             (EDuce.Xhtml.Option ({{ { } }}, value, None, true) )
             (List.map
                (fun a -> EDuce.Xhtml.Option ({{ { } }}, a, None, false) )
                alts
             )
        :} ] }}




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


  method private display_noscript ~sp ~root_task =
    let fields =
      ["length" ; "importance" ; "deadline_version" ; "deadline_time" ; "kind" ]
    in
    Data.get_tree ~sp ~root:root_task () >>= fun tree ->
      let show_line ~task = draw_message_title ~sp ~task:task.Types.t_id
                                                          >>= fun snip ->
        Data.get_area ~sp ~area:task.Types.t_area         >>= fun ainfo ->
        Wiki_sql.get_wiki_info_by_id ainfo.Types.r_wiki   >>= fun winfo ->
          (match winfo.Wiki_types.wiki_pages with
          | None -> Lwt.return ({{ [ ] }} : {{ [Xhtmltypes_duce.a*] }})
          | Some(pages) ->
              (match (Services_ht.find_service pages) with
              | None -> Lwt.return ({{ [ ] }} : {{ [Xhtmltypes_duce.a*] }})
              | Some s -> Lwt.return
                    {{ [ {: EDuce.Xhtml.a
                            ~service:s.Services_ht.sources_service
                            ~sp
                        {{ [ 'repository' ] }}
                            ([],(None,(None,None)))
                            :} ] }} ))
                                                         >>= fun repo_link ->
        Lwt_util.map_serial
          (fun field -> Lwt.return ( self#show_static_field ~field ~task ) )
          fields
                                                          >>= fun fields ->
        Lwt.return
            {{ <tr>[
                 <th align="left">[ !{: to_utf8 snip :} ]
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

  method display ~sp ~root_task =
    Lwt.catch
      (fun () ->
         Ocsimore_page.Header.require_header Forum_widgets.forum_css_header sp ;
         (self#display_noscript ~sp ~root_task) >>= fun ns ->
         Ocsimore_page.add_obrowser_header sp ;
         Lwt.return
           ({{ [ <div id="ocsforge_task_tree">
                    [ <noscript
                        id={: "root_task_" ^ Types.string_of_task root_task :}>
                        [ ns ]
                    ]
               ]
            }} : {{ Xhtmltypes_duce.flows }} ))
      (function
         | Types.Tree.Empty_tree -> Lwt.return
             {{ [ <div id="ocsforge_task_tree">[ 'No task in tree' ] ] }}
         | exc -> Lwt.fail exc )

end


class task_widget (message_widget : Forum_widgets.message_widget) =
object (self)

  method auto_update_select
         ~id
         ~string_of_t ?label_of_t ?value_of_t
         ~value ~alts
         ~name ~service
         ()
         =
    let option_of_t v =
      match label_of_t, value_of_t with
        | None  , None   ->
            {{ [ <option>[ !{: to_utf8 (string_of_t v) :} ] ] }}
        | Some f, None   ->
            {{ [ <option label={: to_utf8 (f v) :}>
                   [ !{: to_utf8 (string_of_t v) :} ]
            ] }}
        | Some f, Some g ->
            {{ [ <option label={: to_utf8 (f v) :}
                         value={: to_utf8 (g v) :}>
                   [ !{: to_utf8 (string_of_t v) :} ]
            ] }}
        | None  , Some g ->
            {{ [ <option value={: to_utf8 (g v) :}>
                   [ !{: to_utf8 (string_of_t v) :} ]
            ] }}
    in
    {{
      <select id={: to_utf8 ( name ^ Int32.to_string id ) :} 
              onchange={:
                Ocsforge_client_calls.keep_up_to_date
                  name id service
              :}>
         {{ List.fold_left
              (fun
                 ( a : {{ [ Xhtmltypes_duce.option+ ] }} )
                 ( b : {{ [ Xhtmltypes_duce.option ] }} )
                 -> {{ [ !a !b ] }} )
              ( option_of_t value )
              ( List.map option_of_t alts )
         }}
    }}


  method display ~sp ~task =

    Data.get_task ~sp ~task                  >>= fun ti ->
    message_widget#display ~sp
      ?classes:( Some [ "ocsforge_task_message" ] )
      ~data:ti.Types.t_message
      ()                                     >>= fun msg ->

    Lwt.return
      ({{ [
        <div>[
          msg
          <div class={: to_utf8 "task_details" :}>[
             !{: to_utf8 "progress : " :}
             {{ self#auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:(
                    Olang.string_of_t_opt Int32.to_string
                  )
                  ~label_of_t:(
                    Olang.string_of_t_opt (Printf.sprintf "%ld %%")
                  )
                  ~value:ti.Types.t_progress
                  ~alts:(
                    Olang.t_opt_list_of_t_list
                      (Olang.int32_interval_list
                         ~bump:5l ~min:0l ~max:100l ()
                      )
                  )
                  ~name:"progress"
                  ~service:"ocsforge_set_progress"
                  () }}
          ]
        ]
      ] }} : {{ Xhtmltypes_duce.flows }})


end
