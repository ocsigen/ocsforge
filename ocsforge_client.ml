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

  let form = new AXOToolkit.block_container in
  let popup = new AXOToolkit.popup form in

  let title_input = new AXOToolkit.text_input "Enter your title here" in
    title_input#set_attribute "size" "100" ;
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_task") ;
                ("parent", Int32.to_string id) ;
                ("subject", title_input#get_value) ;
                ("text", "") ;
                ("length", "") ;
                ("progress", "") ;
                ("importance", "") ;
                ("deadline_t", "") ;
                ("deadline_v", "") ;
                ("kind", "") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "task successfully added")
             (c,m) ;
      ) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( new AXOToolkit.br ) ;
    form#add_common ( save_button      :> AXOWidgets.common ) ;

    popup#show

let edit_task_popup
      id subject length progress importance deadline_t deadline_v kind
      =
  let form = new AXOToolkit.block_container in
  let popup = new AXOToolkit.popup form in

  let title_input = new AXOToolkit.text_input subject in
    title_input#set_attribute "size" "100" ;
  let progress_input =
    new AXOToolkit.select
      (LOption.string_of_t_opt Int32.to_string)
      (LOption.t_opt_of_string Int32.of_string)
      progress
      (LList.t_opt_list_of_t_list
         (LList.int32_interval_list
            ~bump:5l ~min:0l ~max:100l
            ()
         ))
  in
  let importance_input =
    new AXOToolkit.select
      (LOption.string_of_t_opt Int32.to_string)
      (LOption.t_opt_of_string Int32.of_string)
      importance
      (LList.t_opt_list_of_t_list
         (LList.int32_interval_list
            ~bump:5l ~min:0l ~max:100l
            ()
         ))
  in
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [
                ( "__eliom_na__name","ocsforge_add_task") ;
                ( "parent",  Int32.to_string id    ) ;
                ( "subject", title_input#get_value ) ;
                ( "text", "") ;
                ( "length", "") ;
                ( "progress",
                   LOption.string_of_t_opt
                     Int32.to_string
                     progress_input#get_value) ;
                ( "importance",
                   LOption.string_of_t_opt
                     Int32.to_string
                     importance_input#get_value) ;
                ( "deadline_t", "") ;
                ( "deadline_v", "") ;
                ( "kind", "") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "task successfully edited")
             (c,m) ;
      ) ;

    List.iter (fun c -> form#add_common c)
      [
        (                                  title_input :> AXOWidgets.common ) ;
        (                            new AXOToolkit.br                      ) ;
        (   (new AXOToolkit.inline_text "progress : ") :> AXOWidgets.common ) ;
        (                               progress_input :> AXOWidgets.common ) ;
        (                            new AXOToolkit.br                      ) ;
        ( (new AXOToolkit.inline_text "importance : ") :> AXOWidgets.common ) ;
        (                             importance_input :> AXOWidgets.common ) ;
        (                            new AXOToolkit.br                      ) ;
        (                                  save_button :> AXOWidgets.common ) ;
      ]


let tree_of_dom_tree get_content get_children dt =
  let rec aux dt =
    {
      LTree.content  = get_content  dt  ;
      LTree.children = List.map aux (get_children dt) ;
    }
  in
    aux dt

type task = (*As there's no Calendar lib aviable, length is in hours, deadline is a number of days (relative to the current one) *)
    { id : int32     ; msg : AXOWidgets.generic_widget ;
      length : int   ; progress : int32        ; importance : int32  ;
      deadline : int ; milestone : string      ; kind : string       ;
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
  (try AXOCom.check_for_error tree with Failure s -> AXOJs.blunt_alert s) ;
  AXOJs.blunt_alert (tree >>> AXOCom.print_xml) ;
  let tree =
    let get_attr n o = AXOJs.blunt_alert (o >>> AXOCom.print_xml) ; AXOJs.Node.get_attribute n o in
    tree_of_dom_tree
      (fun o ->
         let sid = o >>> get_attr "id" in
         AXOJs.blunt_alert sid ;
         let msg_ = new AXOWidgets.widget_wrap (o >>> AXOJs.Node.child 0) in
         msg_#set_attribute "id" ("task" ^ sid) ;
         {
                  msg = msg_ ;
               length = (o >>> get_attr "length"    ) >>> int_of_string   ;
             progress = (o >>> get_attr "progress"  ) >>> Int32.of_string ;
           importance = (o >>> get_attr "importance") >>> Int32.of_string ;
             deadline = (o >>> get_attr "deadline"  ) >>> int_of_string   ;
            milestone = (o >>> get_attr "milestone" ) ;
                 kind = (o >>> get_attr "kind"      ) ;
                   id = Int32.of_string sid ;
         } )
      (fun o ->
         if o >>> AXOJs.Node.n_children = 2
         then (o >>> AXOJs.Node.child 1) >>> AXOJs.Node.children
         else [] )
      (tree >>> JSOO.get "documentElement" >>> JSOO.get "firstChild")
  in
    tree

let main_tree root_task =
  let task_tree = get_task_tree root_task in
  let dom_tree =
    AXOToolkit.foldable_tree ~depth:3 ~persistent_as_container:true task_tree
      (fun t l f ->
         let main = new AXOToolkit.li_container in
           main#add_common (t.msg :> AXOWidgets.common) ;
         ((if l = []
           then
             ( (new AXOToolkit.inline_text_widget_button ~activated:false "x "
               ) :> AXOWidgets.generic_button )
           else
             ( (if f
                then new AXOToolkit.cyclic_block_text_button "+ " ["- "]
                else new AXOToolkit.cyclic_block_text_button "- " ["+ "]
               ) :> AXOWidgets.generic_button )
          ),
          (new AXOToolkit.li_container),
          (new AXOToolkit.ul_container)
         )
      
      )
      (new AXOToolkit.ul_container)
  in
  let root = AXOJs.Node.body >>> AXOJs.Node.get_element_by_id "ocsforge_task_tree" in
  let container = new AXOWidgets.container_wrap root in
    container#add_common (dom_tree :> AXOWidgets.common)

let _ = Eliom_obrowser_client.register_closure 189 main_tree
let _ = AXOJs.alert ".uue loaded"
