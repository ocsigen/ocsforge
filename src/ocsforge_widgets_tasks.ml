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

(* @author Raphael Proust *)



(* force and compose *)
let (!!) = Lazy.force
let (@@) f g = fun x -> f (g x)

(* module aliases *)
module Olang = Ocsforge_lang
module Roles = Ocsforge_roles
module Types = Ocsforge_types
module Data  = Ocsforge_data
module Services_ht = Ocsforge_services_hashtable

module Params = Eliom_parameters
module FTypes = Forum_types

open Eliom_pervasives
(* provides Calendar and Period module *)
open CalendarLib

(* CSS hook *)
let tree_css_header =
  Page_site.Header.create_header
    (fun () -> [Eliom_output.Html5.css_link
		(Page_site.static_file_uri ["ocsforge_tasks.css"]) ()])
let add_tree_css_header () =
  Page_site.Header.require_header tree_css_header

let draw_message_title ~task =
  Data.find_subject ~task (*FIXME: should not be necessary... but some function needs a string !*)

(* TODO: convert
(* A select field that automatically updates new values
   [id] is the task id ;
   [salt] is to make the identifier unique (in case there are two services with the same parameters name) ;
   [string_of_t] is used to change the [value] (and the [alts]) into what's inserted in option node ;
   [label_of_t] can be specified to have label on options ;
   [value_of_t] can be used to have options carying a label attribute ;
   [value] is the initial value for the select ;
   [alts] is a list of alternatives the user may select ;
   [name] is the name of the service argument ;
   [service] is the service name

   (TODO: This is very low level and should be replaced by eliom client side services...)
*)
let auto_update_select ~id ?(salt = "")
      ~string_of_t ?label_of_t ?value_of_t
      ~value ~alts
      ~name ~service
      ()
      =
  let option_of_t v =
    match label_of_t, value_of_t with
      | None  , None   ->
          {{ [ <option>[ !{: utf8 (string_of_t v) :} ] ] }}
      | Some f, None   ->
          {{ [ <option label={: utf8 (f v) :}>
                 [ !{: utf8 (string_of_t v) :} ]
          ] }}
      | Some f, Some g ->
          {{ [ <option label={: utf8 (f v) :}
                       value={: utf8 (g v) :}>
                 [ !{: utf8 (string_of_t v) :} ]
          ] }}
      | None  , Some g ->
          {{ [ <option value={: utf8 (g v) :}>
                 [ !{: utf8 (string_of_t v) :} ]
          ] }}
  in
  {{
    <select id={: utf8 ( salt ^ name ^ Int32.to_string id ) :} 
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

(* This function makes text input with an automated save (using onchange event).
   It behaves like auto_update_select and share his arguments with it.
   (TODO: call the service directly with eliom
 *)
let auto_update_input ~id ?(salt = "") ~value ~name ~service () =
  {{
    <input id={: utf8 ( salt ^ name ^ Int32.to_string id ) :} 
           onchange={:
                 Ocsforge_client_calls.keep_up_to_date name id service salt
            :}
           type="text"
           value={: value :}>[]
  }}


(** This class has a [display] method that
    - renders a "noscript" in the page (for search engines)
    - passes some info to AXO (via an id attribute)
    (FIXME: information passing is to be changed with a higher level, cleaner, more appropriate way of doing it.
 *)
class tree_widget =
object (self)

  val task_text_class = "ocsforge_task_text"

  (* prepare the table header *)
  val header =
    ({{ [ <thead>[
            <tr>[
              <th>[ ' tasks'       ]
              <th>[ ' repository ' ]
              <th>[ ' duration '   ]
              <th>[ ' progress '   ]
              <th>[ ' importance ' ]
              <th>[ ' kind '       ]
            ]
    ] ] }} : {{ [ Xhtmltypes_duce.thead ] }} )

  (* prepare attribute cells *)
  method private show_static_fields t =
     let f = Olang.string_of_t_opt in
     Lwt.return
      ({{ [
        <td>[ !{: f Olang.string_of_period t.Types.t_length :} ]
        <td>[ !{: f Int32.to_string t.Types.t_progress      :} ]
        <td>[ !{: f Int32.to_string t.Types.t_importance    :} ]
        <td>[ !{: f (fun k -> k) t.Types.t_kind             :} ]
       ] }} : {{ [ Xhtmltypes_duce.td + ] }})

  (* prepare the noscript element with a table inside *)
  method private display_noscript ~sp ~root_task =

    let show_line ~task =
      draw_message_title ~sp ~task:task.Types.t_id          >>= fun snip ->
      Data.get_area ~sp ~area:task.Types.t_area             >>= fun ainfo ->
      Wiki_sql.get_wiki_info_by_id ainfo.Types.r_wiki       >>= fun winfo ->
      (match winfo.Wiki_types.wiki_pages with
         | None -> Lwt.return {{ <td>[ ] }}
         | Some pages ->
             (match Services_ht.find_service pages with
                | None -> Lwt.return {{ <td>[ ] }}
                | Some s -> Lwt.return
                      {{ <td>[ {: EDuce.Xhtml.a ~sp
                                    ~service:s.Services_ht.sources_service
                                    {{ [ 'repository' ] }}
                                    ([], (None, (None, None) ) )
                      :} ] }}))
        >>= fun (repo_link : {{ Xhtmltypes_duce.td }}) ->
      self#show_static_fields task                         >>= fun fields ->
      Lwt.return
          ({{ <tr>[
                <th align="left">[ !{: utf8 snip :} ]
                repo_link
                !fields
           ] }} : {{ <tr>[ Xhtmltypes_duce.th Xhtmltypes_duce.td+ ] }})
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

      Data.get_tree ~sp ~root:root_task () >>= fun tree ->
      show_tree tree                       >>= fun core ->

        Lwt.return
          ( ( {{ <table>[ !header !core ] }} ) : {{ Xhtmltypes_duce.table }})


  (* The main method.
  * FIXME: have the root_task id beeing transfered in a adapted way. *)
  method display ~sp ~root_task =
    Lwt.catch
      (fun () ->
         (self#display_noscript ~sp ~root_task)          >>= fun noscript ->
         Page_site.add_obrowser_header sp ;
         Lwt.return
           ({{ [ <div id="ocsforge_task_tree">
                   [ <noscript
                       id={: "root_task_" ^ Types.string_of_task root_task :}
                       >[ noscript ]
                   ]
            ] }} : {{ Xhtmltypes_duce.flows }} ) )
      (function
         | Olang.Tree.Empty_tree -> Lwt.return
             {{ [ <div id="ocsforge_task_tree">[ 'No task in tree' ] ] }}
         | exc -> Lwt.fail exc )

end


(* check a right property and return an adapted result
   /!\ lazyness /!\ is used to avoid side effects and database access. *)

let check_right (role_field : bool Lwt.t Lazy.t)
  (right : 'a Lazy.t) (wrong : 'a Lazy.t)
  : 'a Lwt.t =
  !!role_field >>= (fun r -> if r then Lwt.return right else Lwt.return wrong)
  >>= ( Lwt.return @@ (!!) )

(* This version is for Lwt.t values ! *)
let check_right_lwt (role_field : bool Lwt.t Lazy.t)
  (right : 'a Lwt.t Lazy.t) (wrong : 'a Lwt.t Lazy.t)
  : 'a Lwt.t =
  !!role_field >>= (fun r -> if r then Lwt.return right else Lwt.return wrong)
  >>= (!!)


class task_widget
      (message_widget : Forum_widgets.message_widget )
      (thread_widget  : Forum_widgets.thread_widget  ) =
object (self)

  (* print the message w/ or w/o comments according to user rights *)
  method display_message sp ti =
    let data = ti.Types.t_message in
    let class_ = "ocsforge_task_message" in
      thread_widget#display_splitted  ~sp ?classes:( Some [ class_ ] ) ~data ()
      >>= fun (m, c) -> Lwt.return (m, {{ [c] }})


  (* display common task fields (progress, duration,...) with an interactive select if the user happens to be an editor. *)
  method display_task_details sp ti role =

    Data.get_kinds ~sp ~area:ti.Types.t_area >>= fun kalts ->

    check_right role.Roles.task_property_editor (* lazyness avoid db access *)
      (* What to print when the user has edition rights... *)
      (lazy ({{ [
           <div>[
             !{: utf8 "progress : " :}
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
             !{: utf8 " %" :}
           ]
           <div>[
             !{: utf8 "duration : " :}
             {{ auto_update_select
                  ~id:(Types.sql_of_task ti.Types.t_id)
                  ~string_of_t:( Olang.string_of_t_opt Olang.string_of_period )
                  ~value:ti.Types.t_length
                  ~alts:(Olang.t_opt_list_of_t_list
                      (  (Calendar.Period.lmake ~hour:1  ())
                       ::(Calendar.Period.lmake ~hour:2  ())
                       ::(Calendar.Period.lmake ~hour:3  ())
                       ::(Calendar.Period.lmake ~hour:6  ())
                       ::(Calendar.Period.lmake ~hour:12 ())
                       ::(Calendar.Period.lmake ~day:1  ())
                       ::(Calendar.Period.lmake ~day:2  ())
                       ::(Calendar.Period.lmake ~day:7  ())
                       ::(Calendar.Period.lmake ~day:15 ())
                       ::(Calendar.Period.lmake ~month:1 ())
                       ::(Calendar.Period.lmake ~month:3 ())
                       ::(Calendar.Period.lmake ~month:6 ())
                       ::(Calendar.Period.lmake ~year:1 ())
                       ::(Calendar.Period.lmake ~year:5 ())
                       :: [] )
                  )
                  ~name:"length"
                  ~service:"ocsforge_set_length"
                  () }}
           ]
           <div>[
             !{: utf8 "importance : " :}
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
             !{: utf8 "category : " :}
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
      }} : {{ [ Xhtmltypes_duce._div + ] }} ))

      (* What to print when the user doesn't have edition rights *)
      (lazy ({{ [
           <div>[ !{: utf8
             (  "progress : "
              ^ Olang.string_of_t_opt Int32.to_string ti.Types.t_progress
              ^ " %" )
           :} ]
           <div>[ !{: utf8
             (  "duration : "
              ^ Olang.string_of_t_opt Olang.string_of_period ti.Types.t_length
             )
           :} ]
           <div>[ !{: utf8
             (  "importance : "
              ^ Olang.string_of_t_opt Int32.to_string ti.Types.t_importance )
           :} ]
           <div>[ !{: utf8
             (  "category : "
              ^ Olang.string_of_t_opt (fun k -> k) ti.Types.t_kind
             )
           :} ]
        ]
      }} : {{ [ Xhtmltypes_duce._div + ] }} ))



  (* Show a task with message and detailed information *)
  method display_task ~sp ti role =
    self#display_message sp ti           >>= fun (msg, comments) ->
    self#display_task_details sp ti role >>= fun details ->
    add_tree_css_header sp ;
    Lwt.return
      ({{ [ <div>[
              msg
              <div class={: utf8 "ocsforge_task_details" :}>
                 [ <h4>{: utf8 "task properties : " :} !details ]
              !comments
      ] ] }} : {{ Xhtmltypes_duce.flows }})



  (* Not implememted yet : show fields to tamper with aviable categories.
     FIXME: implement with Eliom client side services... (using Ocsforge_services.{add|del|swap}_area_kinds_service)*)
  method display_kind_options sp ti role =
    Data.get_kinds ~sp ~area:ti.Types.t_area >>= fun _kalts ->
    check_right role.Roles.kinds_setter
      (lazy ({{ [<div class={: utf8 "ocsforge_categories_details" :}>[
                 <div>[ !{: utf8 "add category : TODO" :} ]
                 <div>[ !{: utf8 "remove categories : TODO" :} ]
                 <div>[ !{: utf8 "swap categories : TODO" :} ]
      ] ] }} : {{ [ Xhtmltypes_duce._div ] }}))
      (lazy ({{ [ ] }}))


  (* Show editable repository details (if user is in repo_setter) *)
  method display_repository_options sp ti role =
    Data.get_area_for_task ~sp ~task:ti.Types.t_id  >>= fun ai ->
    check_right role.Roles.repository_setter
      (lazy ({{ [
        !{: utf8 "change repository : " :}
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
        !{: utf8 ":" :}
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
      ] }} : {{ Xhtmltypes_duce.flows }} ))
      (lazy ( {{ [ ] }} : {{ Xhtmltypes_duce.flows }} ))


  method display_source_link sp ti =
    Data.get_area ~sp ~area:ti.Types.t_area               >>= fun ainfo ->
    Wiki_sql.get_wiki_info_by_id ainfo.Types.r_wiki       >>= fun winfo ->
      match winfo.Wiki_types.wiki_pages with
        | None -> Lwt.return ( {{ [ ] }} : {{ [ Xhtmltypes_duce.a* ] }} )
        | Some pages ->
            match Services_ht.find_service pages with
              | None -> Lwt.return ({{ [ ] }} : {{ [ Xhtmltypes_duce.a* ] }})
              | Some s -> Lwt.return
                    {{ [ {: EDuce.Xhtml.a ~sp
                            ~service:s.Services_ht.sources_service
                            {{ [ 'browse repository' ] }}
                            ([], (None, (None, None) ) )
                    :} ] }}


  method display_project ~sp ti role =

    self#display_message sp ti                 >>= fun (msg, comments) ->
    self#display_task_details sp ti role       >>= fun details ->
    self#display_source_link sp ti             >>= fun src_lnk ->
(*    self#display_kind_options sp ti role       >>= fun kinds -> *)
    self#display_repository_options sp ti role >>= fun repo ->
    add_tree_css_header sp ;

    Lwt.return
      ( {{ [ <div>[
                msg
                <div class={: utf8 "ocsforge_repository_details" :}>
                   [ <h4>{: utf8 "Repository : " :} !src_lnk <br>[] !repo ]
                <div class={: utf8 "ocsforge_task_details" :}>[
                  <h4>{: utf8 "task properties : " :} !details ]
                  !comments
                (* !kinds *)
        ] ] }}
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
*)
