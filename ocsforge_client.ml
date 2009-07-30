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

open AXOLang
let (>>>) x f = f x


(*poping up the new task form*)
let new_task_pop_up id =

  (* Base bricks *)
  let form = new AXOToolkit.vbox in
  let popup = new AXOToolkit.popup form in
  let details = new AXOToolkit.block_widget_container in
  let more = new AXOToolkit.block_foldable
    ( (new AXOToolkit.cyclic_inline_text_button
         "More options" [ "Less options" ]
      ) :> AXOWidgets.generic_button)
    (new AXOToolkit.inline_container)
    (details :> AXOWidgets.generic_container)
  in

  (* "More options" fields *)
  let importance_select =
    new AXOToolkit.select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
  in
  let progress_select =
    new AXOToolkit.select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
  in
  let length_select =
    new AXOToolkit.select
      (LOption.string_of_t_opt (fun t -> (string_of_int t) ^ " hours"))
      (LOption.t_opt_of_string
         (fun s -> Scanf.sscanf s "%i hours" (fun i -> i)))
      None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:6 ~min:0 ~max:48 ()))

  in
(*let deadline_select =
  in
  let milestone_select =
  in
  let kind_select =
  in  *)

  (* populating details and fixing some visual effects *)
  more#set_margin_left 10 ;
  details#set_style_property "border" "1px solid" ;
  details#add_common (AXOToolkit.text "importance : ") ;
  details#add_common (importance_select :> AXOWidgets.common) ;
  details#add_common (new AXOToolkit.br) ;
  details#add_common (AXOToolkit.text "progress : ") ;
  details#add_common (progress_select :> AXOWidgets.common) ;
  details#add_common (new AXOToolkit.br) ;
  details#add_common (AXOToolkit.text "length : ") ;
  details#add_common (length_select :> AXOWidgets.common) ;

  let title_input = new AXOToolkit.text_input "Enter your title here" in
    title_input#set_attribute "size" "100" ;
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_task") ;
                ("parent", string_of_int id) ;
                ("subject", title_input#get_value) ;
                ("text", "") ;
                ("length",
                 LOption.string_of_t_opt
                   string_of_int
                   length_select#get_value) ;
                ("progress",
                 LOption.string_of_t_opt
                   string_of_int
                   progress_select#get_value) ;
                ("importance",
                 LOption.string_of_t_opt
                   string_of_int
                   importance_select#get_value) ;
                ("deadline_t", "") ;
                ("deadline_v", "") ;
                ("kind", "") ;
                ("repo_kind","") ;
                ("repo_path","") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "task successfully added")
             ~on_4xx:(
               fun (_,m) ->
                 AXOJs.rich_alert
                   ((AXOCom.parse_xml m) >>> JSOO.get "documentElement")
             )
             ~on_5xx:(
               fun (_,m) ->
                 AXOJs.rich_alert
                   ((AXOCom.parse_xml m) >>> JSOO.get "documentElement")
             )
             (c,m) ;
      ) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( save_button      :> AXOWidgets.common ) ;

    popup#show

let new_project_popup id =
  let form = new AXOToolkit.vbox in
  let popup = new AXOToolkit.popup form in
  let title_input = new AXOToolkit.text_input "Enter a name for your project" in
    title_input#set_attribute "size" "50" ;
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_project") ;
                ("parent", string_of_int id) ;
                ("name", title_input#get_value) ;
                ("length","") ;
                ("progress","") ;
                ("importance","") ;
                ("deadline_t", "") ;
                ("kind", "") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "project successfully added")
             ~on_4xx:(
               fun (_,m) ->
                 AXOJs.rich_alert
                   ((AXOCom.parse_xml m) >>> JSOO.get "documentElement")
             )
             ~on_5xx:(
               fun (_,m) ->
                 AXOJs.rich_alert
                   ((AXOCom.parse_xml m) >>> JSOO.get "documentElement")
             )
             (c,m) ;
      ) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( save_button      :> AXOWidgets.common ) ;

    popup#show


let tree_of_dom_tree get_content get_children dt =
  let rec aux dt =
    {
      LTree.content  = get_content  dt  ;
      LTree.children = List.map aux (get_children dt) ;
    }
  in
    aux dt

(*TODO: use int32 fields (when string conversions works !)*)
type task = (*As there's no Calendar lib aviable, length is in hours, deadline is a number of days (relative to the current one) *)
    { id : int                ; sub : string          ;
      length : int option     ; progress : int option ;
      importance : int option ; deadline : int option ;
      milestone : string      ; kind : string         ;
    }


let get_task_tree root_task (*TODO: limit depth and dynamicly load the remaining branches*)=
  let tree = (*TODO: catch error on 1xx, 3xx, 4xx, 5xx*)
    AXOCom.dynload_post "./" 
      [
        ("__eliom_na__name", "ocsforge_task_dump") ;
        ("root", Int32.to_string root_task) ;
        ("format", "xml") ;
        (* ("depth", "3") ; *)
      ]
      AXOCom.parse_xml
  in
  AXOCom.check_for_error tree ;
  let tree =
    let get_attr n o =
      o >>> JSOO.get "attributes" >>> JSOO.get n >>> JSOO.get "value" >>> JSOO.as_string
    in
    tree_of_dom_tree
      (fun o ->
         let sub_ = (o >>> AXOJs.Node.child 0)
                       >>> JSOO.get "textContent"
                       >>> JSOO.as_string
         in
         {
                  sub = sub_ ;
             progress = (o >>> get_attr "progress"  )
                           >>> LOption.t_opt_of_string int_of_string ;
           importance = (o >>> get_attr "importance")
                           >>> LOption.t_opt_of_string int_of_string ;
             deadline = (o >>> get_attr "deadline"  )
                           >>> LOption.t_opt_of_string int_of_string   ;
            milestone = (o >>> get_attr "milestone" ) ;
                 kind = (o >>> get_attr "kind"      ) ;
               length = (o >>> get_attr "_length_" )
                           >>> LOption.t_opt_of_string int_of_string   ;
                   id = (o >>> get_attr "id" ) >>> int_of_string ;
         } )
      (fun o ->
         if o >>> AXOJs.Node.n_children = 2
         then (o >>> AXOJs.Node.child 1) >>> AXOJs.Node.children
         else [] )
      (tree >>> JSOO.get "documentElement")
  in
    tree

let make_rw_line t =
  (* The whole line container *)
  let main = new AXOToolkit.li_widget_container in

  (* The subject *)
  let subject = AXOToolkit.text t.sub in

  (* The other columns *)
  let columns = new AXOToolkit.inline_container in
  let new_button = new AXOToolkit.inline_text_widget_button "NEW" in
    new_button#add_click_action ( fun () -> new_task_pop_up t.id None ) ;
    new_button#set_position AXOWidgets.Absolute ;
    new_button#set_x 0 ;
  let new_project = new AXOToolkit.inline_text_widget_button "NEW PROJECT" in
    new_project#add_click_action (fun () -> new_project_popup t.id None ) ;
    new_project#set_position AXOWidgets.Absolute ;
    new_project#set_x 50 ;
  let importance = new AXOToolkit.auto_update_select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.importance
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
      "importance" "./"
      [
        ("id", string_of_int t.id) ;
        ("__eliom_na__name", "ocsforge_set_importance")
      ]
  in
    importance#set_position AXOWidgets.Absolute ;
    importance#set_x 150 ;
  let progress = new AXOToolkit.auto_update_select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.progress
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
      "progress" "./"
      [
        ("id", string_of_int t.id) ;
        ("__eliom_na__name", "ocsforge_set_progress")
      ]
  in
    progress#set_position AXOWidgets.Absolute ;
    progress#set_x 200 ;


    columns#add_common ( new_button :> AXOWidgets.common ) ;
    columns#add_common ( new_project:> AXOWidgets.common ) ;
    columns#add_common ( importance :> AXOWidgets.common ) ;
    columns#add_common ( progress   :> AXOWidgets.common ) ;

  (* The mash up *)
    main#set_style_property "listStyleType" "none" ;
    main#add_common (columns :> AXOWidgets.common) ;
    main#add_common (subject :> AXOWidgets.common) ;
    main

let main_tree root_task =
  let task_tree = get_task_tree root_task in
    AXOToolkit.foldable_tree ~depth:3 ~persistent_as_container:true task_tree
      (fun t l f ->
         ((if l = []
           then
             ( (new AXOToolkit.inline_text_widget_button ~activated:false "x "
               ) :> AXOWidgets.generic_button )
           else
             ( (if f
                then new AXOToolkit.cyclic_inline_text_button "+ " ["- "]
                else new AXOToolkit.cyclic_inline_text_button "- " ["+ "]
               ) :> AXOWidgets.generic_button )
          ),
          ((make_rw_line t) :> AXOWidgets.generic_container),
          (let u = new AXOToolkit.ul_widget_container in
             u#set_style_property "margin" "0px" ;
             (u :> AXOWidgets.generic_container)
          )
         )
      
      )
      (new AXOToolkit.block_container)


let show_window task =
  let c = new AXOToolkit.vbox in

    let t = new AXOToolkit.inline_widget_container in
      t#set_style_property "marginLeft" "50%" ;
      t#add_common (let tt = new AXOToolkit.inline_text "importance" in
                      tt#set_position AXOWidgets.Absolute ;
                      tt#set_x 150 ;
                      tt :> AXOWidgets.common ) ;
      t#add_common (let tt = new AXOToolkit.inline_text "progress" in
                      tt#set_position AXOWidgets.Absolute ;
                      tt#set_x 200 ;
                      tt :> AXOWidgets.common ) ;
      t#add_common (AXOToolkit.text "Tasks") ;
    c#add_common ( t :> AXOWidgets.common) ;

    let m = main_tree task in
      m#set_style_property "marginLeft" "50%" ;
    c#add_common ( m :> AXOWidgets.common ) ;

    AXOWidgets.body#add_common (c :> AXOWidgets.common)

let _ = Eliom_obrowser_client.register_closure 189 show_window

