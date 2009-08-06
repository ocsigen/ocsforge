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
let (@@) f g = fun x -> f (g x)

module Olang = Ocsforge_lang
module Roles = Ocsforge_roles
module Types = Ocsforge_types
module Data  = Ocsforge_data

module Services_ht = Ocsforge_services_hashtable
module Params = Eliom_parameters
module EDuce = Eliom_duce
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

let auto_update_select ~id ?(salt = "")
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
    <select id={: to_utf8 ( salt ^ name ^ Int32.to_string id ) :} 
            onchange={:
                 Ocsforge_client_calls.keep_up_to_date name id service salt
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

let auto_update_input ~id ?(salt = "") ~value ~name ~service () =
  {{
    <input id={: to_utf8 ( salt ^ name ^ Int32.to_string id ) :} 
           onchange={:
                 Ocsforge_client_calls.keep_up_to_date name id service salt
            :}
           type="text"
           value={: value :}>[]
  }}


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
                 | "length" ->
                     f Olang.string_of_period t.Types.t_length
                 | _ -> f (fun _ -> "") None)
        :}
    }}


  method private display_noscript ~sp ~root_task =
    let fields =
      ["length" ; "progress" ; "importance" ; "kind" ]
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
      let rec show_tree { Olang.Tree.content = t ; Olang.Tree.children = l } =
        (show_line ~task:t                                    >>= fun a ->
        Lwt_util.map_serial (fun tree -> show_tree tree) l    >>= fun b ->
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
         | Olang.Tree.Empty_tree -> Lwt.return
             {{ [ <div id="ocsforge_task_tree">[ 'No task in tree' ] ] }}
         | exc -> Lwt.fail exc )

end



let check_right (role_field : bool Lwt.t Lazy.t)
  (granted : 'a) (refused : 'a) : 'a Lwt.t =
    !!role_field >>= fun right ->
    Lwt.return (if right then granted else refused)


class task_widget
      (message_widget : Forum_widgets.message_widget)
      (thread_widget : Forum_widgets.thread_widget) =
object (self)
    (*TODO: put the fields between first message and comments ; lot of CSS ;
     * check rights ! *)

  method display_message sp ti role =
    let data = ti.Types.t_message and class_ = "ocsforge_task_message" in
    check_right role.Roles.task_comment_reader
      (lazy (thread_widget#display  ~sp ?classes:( Some [ class_ ] ) ~data ()))
      (lazy (message_widget#display ~sp ?classes:( Some [ class_ ] ) ~data ()))
    >>= (!!)

  method display_task_details sp ti role =
    Data.get_kinds ~sp ~area:ti.Types.t_area >>= fun kalts ->
    check_right role.Roles.task_property_editor
      {{ (* case for editors : fields are auto_updating *)
        <div class={: to_utf8 "task_details" :}>[
           <div>[
             !{: to_utf8 "progress : " :}
             {{ auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:( Olang.string_of_t_opt Int32.to_string )
                  ~value:ti.Types.t_progress
                  ~alts:(Olang.t_opt_list_of_t_list
                      (Olang.int32_interval_list ~bump:5l ~min:0l ~max:100l ())
                  )
                  ~name:"progress"
                  ~service:"ocsforge_set_progress"
                  () }}
             !{: to_utf8 " %" :}
           ]
           <div>[
             !{: to_utf8 "duration : " :}
             {{ auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:( Olang.string_of_t_opt Olang.string_of_period  )
                  ~value_of_t:( Olang.string_of_t_opt
                      (string_of_int @@ Olang.hours_in_period)
                  )
                  ~value:ti.Types.t_length
                  ~alts:(Olang.t_opt_list_of_t_list
                      ( (Olang.period_interval_list
                           ~min:(Calendar.Period.lmake ~hour:1 ())
                           ~max:(Calendar.Period.lmake ~hour:23 ())
                           ())
                       @(Olang.period_interval_list
                           ~bump:(Calendar.Period.lmake ~day:1 ())
                           ~min:(Calendar.Period.lmake ~day:1 ())
                           ~max:(Calendar.Period.lmake ~day:7 ())
                           () ) )
                  )
                  ~name:"length"
                  ~service:"ocsforge_set_length"
                  () }}
           ]
           <div>[
             !{: to_utf8 "importance : " :}
             {{ auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:( Olang.string_of_t_opt Int32.to_string )
                  ~value:ti.Types.t_importance
                  ~alts:( Olang.t_opt_list_of_t_list
                      (Olang.int32_interval_list ~bump:5l ~min:0l ~max:100l ())
                  )
                  ~name:"importance"
                  ~service:"ocsforge_set_importance"
                  () }}
           ]
           <div>[
             !{: to_utf8 "category : " :}
             {{ auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:( Olang.string_of_t_opt (fun k -> k) )
                  ~value:ti.Types.t_kind
                  ~alts:( Olang.t_opt_list_of_t_list kalts )
                  ~name:"kind"
                  ~service:"ocsforge_set_kind"
                  () }}
           ]
        ]
      }}
      {{ (* case for readers : only text *)
        <div class={: to_utf8 "task_details" :}>[
           <div>[ !{: to_utf8
             (  "progress : "
              ^ Olang.string_of_t_opt Int32.to_string ti.Types.t_progress
              ^ " %" )
           :} ]
           <div>[ !{: to_utf8
             (  "duration : "
              ^ Olang.string_of_t_opt
                  (string_of_int @@ Olang.hours_in_period)
                  ti.Types.t_length
              ^ " hours" )
           :} ]
           <div>[ !{: to_utf8
             (  "importance : "
              ^ Olang.string_of_t_opt Int32.to_string ti.Types.t_importance )
           :} ]
           <div>[ !{: to_utf8
             (  "category : "
              ^ Olang.string_of_t_opt (fun k -> k) ti.Types.t_kind
             )
           :} ]
        ]
      }}

  method display_task ~sp ti role =

    self#display_message sp ti role >>= fun msg ->
    self#display_task_details sp ti role >>= fun details ->

    Lwt.return ({{ [ <div>[ details msg ] ] }} : {{ Xhtmltypes_duce.flows }})


  method display_kind_options sp ti role =
    Data.get_kinds ~sp ~area:ti.Types.t_area >>= fun kalts ->
    check_right role.Roles.kinds_setter
      {{ [<div>[
           <div>[
             !{: to_utf8 "add categories : TODO" :}
           ]
           <div>[
             !{: to_utf8 "remove categories : TODO" :}
           ]
           <div>[
             !{: to_utf8 "swap categories : TODO" :}
           ]
      ] ] }}
      {{ [ ] }}

  method display_repository_options sp ti role =
    Data.get_area_for_task ~sp ~task:ti.Types.t_id  >>= fun ai ->
    check_right role.Roles.repository_setter
      {{ [<div>[
           !{: to_utf8 "source repository : " :}
           {{ auto_update_select
                ~id:(Types.sql_of_task ti.Types.t_id)
                ~salt:"repo_"
                ~string_of_t:( Olang.string_of_t_opt (fun k -> k) )
                ~value:ai.Types.r_repository_kind
                ~alts:(Olang.t_opt_list_of_t_list
                         (Ocsforge_version_managers.get_managers_list ())
                )
                ~name:"kind"
                ~service:"ocsforge_set_repository_kind"
                ()
           }}
           !{: to_utf8 ":" :}
           {{ auto_update_input
                ~id:(Types.sql_of_task ti.Types.t_id)
                ~salt:"repo_"
                ~value:(Olang.string_of_t_opt (fun k -> k)
                          ai.Types.r_repository_path
                )
                ~name:"path"
                ~service:"ocsforge_set_repository_path"
                ()

           }}
      ] ] }}
      {{ [ ] }}

  method display_project ~sp ti role =

    Data.get_kinds ~sp ~area:ti.Types.t_area        >>= fun kalts ->
    self#display_message sp ti role            >>= fun msg ->
    self#display_task_details sp ti role       >>= fun details ->
(*    self#display_kind_options sp ti role       >>= fun kinds -> *)
    self#display_repository_options sp ti role >>= fun repo ->

    Lwt.return
      (  {{ [ <div>[ msg details (* !kinds *) !repo ] ] }}
       : {{ Xhtmltypes_duce.flows }})


  method display ~sp ~task =
    Data.get_task ~sp ~task                  >>= fun ti ->
    Roles.get_area_role ~sp ti.Types.t_area  >>= fun role ->
    !!( role.Roles.task_reader )             >>= fun read ->
    if read
    then
      (if ti.Types.t_area_root
       then self#display_project
       else self#display_task
      ) sp ti role
    else Lwt.fail Ocsimore_common.Permission_denied


end
