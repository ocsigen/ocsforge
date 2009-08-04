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


(********************************)
(*** Source display functions ***)
(********************************)

let get_path () =
  let s = (AXOJs.Misc.get_location ()) >>> JSOO.get "pathname" >>> JSOO.as_string in
    try
      (Regexp.exec (Regexp.make "/(.*)/sources.*") s).(1)
    with exc -> (AXOJs.alert (Printexc.to_string exc) ; raise exc)
let get_version () = (*TODO: make HL version aviable in AXO*)
  let s = (AXOJs.Misc.get_location ()) >>> JSOO.get "search" >>> JSOO.as_string in
    try
      if Regexp.test (Regexp.make ".*version=.*") s
      then Some (Regexp.exec (Regexp.make "version=([^&]*)") s).(1)
      else None
    with exc -> (AXOJs.alert (Printexc.to_string exc) ; raise exc)

let get_trs dir =
  let l = (*TODO: catch error on 1xx, 3xx, 4xx, 5xx*)
    AXOCom.dynload_post "./" 
      (   ("__eliom_na__name", "ocsforge_repository_tree")
       :: ("dir",   get_path ()
                  ^ "/"
                  ^ (String.sub dir 1 (pred (String.length dir)))
          )
       :: (match get_version () with | None -> [] | Some s -> [ ("version", s) ])
      )
      AXOCom.parse_xml
  in
  AXOCom.check_for_error l ;
  l >>> JSOO.get "documentElement" >>> AXOJs.Node.children

(*
let _ =
  if Regexp.test
       (Regexp.make "sources")
       (AXOJs.Misc.get_location () >>> JSOO.get "pathname" >>> JSOO.as_string)
  then
  try
    begin
      let tbodies =
         (AXOJs.Node.body >>> JSOO.call_method "getElementsByTagName"
                                [| AXOJs.string "TBODY" |])
      in
    
      for i = 0 to (tbodies >>> JSOO.get "length" >>> JSOO.as_int) do
        try
          begin 
            let tbody = tbodies >>> JSOO.call_method "item" [| JSOO.int i |] in
            let folders =
              (tbody >>> JSOO.call_method "getElementsByClassName"
                            [| AXOJs.string "folder" |])
            in
        
            let len = folders >>> JSOO.get "length" >>> JSOO.as_int in
            if len = 0
            then failwith ""
            else 
              for i = 0 to (folders >>> JSOO.get "length" >>> JSOO.as_int) do
                try
                  begin 
                    let folder = folders >>> JSOO.call_method "item"
                                                [| JSOO.int i |]
                    in
                    let imgs =
                      folder >>> JSOO.call_method "getElementsByTagName"
                                   [| AXOJs.string "IMG" |]
                    in
                    let trs =
                      get_trs (folder >>> JSOO.get "textContent"
                                      >>> JSOO.as_string)
                    in
                    let click img () = 
                      List.iter
                        (fun t -> tbody >>> AXOJs.Node.insert_before
                                    t (folder >>> JSOO.get "nextSibling"))
                        trs ;
                      img >>> AXOEvents.Onclick.clear ()
                    in (*FIXME: imgs is null, but why ?*)
                    for i = 0 to (imgs >>> JSOO.get "length" >>> JSOO.as_int) do
        
                      let img = imgs >>> JSOO.call_method "item"
                                                [| JSOO.int i |]
                      in
                        img >>> AXOEvents.Onclick.bind (click img)
                    done
                  end
                with _ -> ()
              done
          end
        with _ -> ()
      done
    end
  with _ -> ()
*)


(*********************************)
(*** Task management functions ***)
(*********************************)

(*TODO: use int32 fields (when string conversions works !)*)
type task =
  (*As there's no Calendar lib aviable,
    length is in days,
    deadline is a number of days (relative to the current one) *)
    { id : int                ; sub : string          ;
      length : int option     ; progress : int option ;
      importance : int option ; deadline : int option ;
      milestone : string      ; kind : string         ;
      editable : bool         ; project : bool        ;
    }

(*poping up the new task form*)
let rec new_task_pop_up id (pos, x, y) =

  (* Base bricks *)
  let form    = new AXOToolkit.vbox in
  let popup   = new AXOToolkit.popup form in
  let details = new AXOToolkit.block_widget_container in
  let more    = new AXOToolkit.block_foldable
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
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:6 ~min:0 ~max:48 ()))
  in
  let deadline_t_select =
    new AXOToolkit.select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      None
      (LList.t_opt_list_of_t_list
         ( (LList.int_interval_list ~bump:1 ~min:(-1) ~max:7 ())
          @(LList.int_interval_list ~bump:7 ~min:15 ~max:29 ())))
  in
  let deadline_v_input = new AXOToolkit.text_input "" in
  let kind_input = new AXOToolkit.text_input "" in

  (* populating details and fixing some visual effects *)
  more#set_margin_left 10 ;
  details#set_style_property "border" "1px solid" ;
  details#add_common ( AXOToolkit.text "importance : "        ) ;
  details#add_common ( importance_select :> AXOWidgets.common ) ;
  details#add_common ( new AXOToolkit.br                      ) ;
  details#add_common ( AXOToolkit.text "progress : "        ) ;
  details#add_common ( progress_select :> AXOWidgets.common ) ;
  details#add_common ( new AXOToolkit.br                    ) ;
  details#add_common ( AXOToolkit.text "length : "        ) ;
  details#add_common ( length_select :> AXOWidgets.common ) ;
  details#add_common ( AXOToolkit.text " days"            );
  details#add_common ( new AXOToolkit.br                  ) ;
  details#add_common ( AXOToolkit.text "deadline : "          ) ;
  details#add_common ( deadline_t_select :> AXOWidgets.common ) ;
  details#add_common ( AXOToolkit.text " days ahead"          );
  details#add_common ( new AXOToolkit.br                      ) ;
  details#add_common ( AXOToolkit.text "milestone : "        ) ;
  details#add_common ( deadline_v_input :> AXOWidgets.common ) ;
  details#add_common ( new AXOToolkit.br                     ) ;
  details#add_common ( AXOToolkit.text "category : "   ) ;
  details#add_common ( kind_input :> AXOWidgets.common ) ;

  let title_input = new AXOToolkit.text_input "" in
    title_input#set_attribute "size" "100" ;
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#set_margin_left 70 ;
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_task") ;
                ("parent", string_of_int id            ) ;
                ("subject", title_input#get_value      ) ;
                ("text", "") ;

                ("length",
                 LOption.string_of_t_opt string_of_int
                   length_select#get_value                ) ;
                ("progress",
                 LOption.string_of_t_opt string_of_int
                   progress_select#get_value              ) ;
                ("importance",
                 LOption.string_of_t_opt string_of_int
                   importance_select#get_value            ) ;
                ("deadline_t",
                 LOption.string_of_t_opt string_of_int
                   deadline_t_select#get_value            ) ;
                ("deadline_v", deadline_v_input#get_value ) ;
                ("kind", kind_input#get_value             ) ;

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

    let project_button =
      new AXOToolkit.inline_text_widget_button "NEW PROJECT"
    in
      project_button#add_click_action
        (fun () -> popup#hide ; new_project_popup id (pos, x, y)) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( more             :> AXOWidgets.common ) ;
    form#add_common ( let span = new AXOToolkit.inline_container in
                        span#add_common (project_button:> AXOWidgets.common) ;
                        span#add_common (save_button   :> AXOWidgets.common) ;
                        (span :> AXOWidgets.common)
    ) ;

    popup#set_position pos ; popup#set_x x ; popup#set_y y ;
    popup#show

and new_project_popup id (pos, x, y) =
  let form = new AXOToolkit.vbox in
  let popup = new AXOToolkit.popup form in
  let title_input = new AXOToolkit.text_input "" in
    title_input#set_attribute "size" "50" ;
  let repo_kind_select = new AXOToolkit.select
    (LOption.string_of_t_opt (fun k -> k))
    (LOption.t_opt_of_string (fun k -> k))
    None
    [ Some "Darcs" ; Some "SVN" ] (*TODO: have the server pass this list... *)
  in
  let repo_path_input = new AXOToolkit.text_input "" in

  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#set_margin_left 70 ;
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              (LOption.optionnaly_add_to_list
                 (LOption.optionnaly_add_to_list
                    [ ("__eliom_na__name","ocsforge_add_project") ;
                      ("parent", string_of_int id) ;
                      ("name", title_input#get_value) ;
                      ("length","") ;
                      ("progress","") ;
                      ("importance","") ;
                      ("deadline_t", "") ;
                      ("kind", "") ;
                    ]
                    (LOption.apply_on_opted (fun s -> ("repo_kind",s))
                       repo_kind_select#get_value)
                 )
                 (LOption.apply_on_opted (fun s -> ("repo_path",s))
                    (LOption.t_opt_of_string (fun k -> k)
                       repo_path_input#get_value)
                 )
              )
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

    let task_button =
      new AXOToolkit.inline_text_widget_button "NEW TASK"
    in
      task_button#add_click_action
        (fun () -> popup#hide ; new_task_pop_up id (pos, x, y)) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( let span = new AXOToolkit.inline_container in
                        span#add_common (AXOToolkit.text "Repository kind : ") ;
                        span#add_common (repo_kind_select :> AXOWidgets.common);
                        (span :> AXOWidgets.common)
                    ) ;
    form#add_common ( let span = new AXOToolkit.inline_container in
                        span#add_common (AXOToolkit.text "Repository path : ") ;
                        span#add_common (repo_path_input :> AXOWidgets.common) ;
                        (span :> AXOWidgets.common)
                    ) ;
    form#add_common ( new AXOToolkit.br ) ;
    form#add_common ( let span = new AXOToolkit.inline_container in
                        span#add_common (task_button:> AXOWidgets.common) ;
                        span#add_common (save_button   :> AXOWidgets.common) ;
                        (span :> AXOWidgets.common)
    ) ;

    popup#set_position pos ; popup#set_x x ; popup#set_y y ;
    popup#show


let tree_of_dom_tree get_content get_children dt =
  let rec aux dt =
    {
      LTree.content  = get_content  dt  ;
      LTree.children = List.map aux (get_children dt) ;
    }
  in
    aux dt


let task_detail_pop_up t =

  (* Base bricks *)
  let form    = new AXOToolkit.widget_vbox in
  let popup   = new AXOToolkit.popup form in
  let details = new AXOToolkit.widget_vbox in

  (* "More options" fields *)
  let make_field
        string_of_t t_of_string value alts name ?(suffix = "") url args
        not_for_projects
    =
   if not_for_projects && t.project
   then None
   else
     begin
       let b = new AXOToolkit.inline_container in
       b#add_common (AXOToolkit.text (name ^ " : ")) ;
       (if t.editable
        then
          b#add_common
            (new AXOToolkit.auto_update_select
               string_of_t t_of_string value alts name url args
             :> AXOWidgets.common)
        else
          b#add_common
            (AXOToolkit.text (string_of_t value))
       ) ;
       b#add_common (AXOToolkit.text suffix) ; 
       Some (b :> AXOWidgets.common) 
     end 
  in 
  let add_common_option c o = match c with
    | None -> ()
    | Some c -> o#add_common ?before:None c 
  in
 
  let importance =
    make_field 
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.importance
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
      "importance"
      "./"
      [ ( "__eliom_na__name", "ocsforge_set_importance" ) ;
        ( "id", string_of_int t.id) ; ]
      true
  in
  let progress =
    make_field 
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.progress
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
      "progress" ~suffix:"%" 
      "./"
      [ ( "__eliom_na__name", "ocsforge_set_progress" ) ;
        ( "id", string_of_int t.id) ; ]
      false
  in
  let length =
    make_field
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.length
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:6 ~min:0 ~max:48 ()))
      "length" ~suffix:" days"
      "./"
      [ ( "__eliom_na__name", "ocsforge_set_length" ) ;
        ( "id", string_of_int t.id) ; ]
      false
  in
  let deadline_t =
    make_field
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      t.deadline
      (LList.t_opt_list_of_t_list
         ( (LList.int_interval_list ~bump:1 ~min:(-1) ~max:7 ())
          @(LList.int_interval_list ~bump:7 ~min:15 ~max:29 ())))
      "deadline_t" ~suffix:" days ahead"
      "./" 
      [ ( "__eliom_na__name", "ocsforge_set_deadline_t" ) ;
        ( "id", string_of_int t.id) ; ]
      false
  in

  (* populating details and fixing some visual effects *)
  details#set_margin_left 10 ;
  details#set_style_property "border" "1px solid" ;
  details#set_position AXOWidgets.Relative ;
  details#set_background "white" ;
  details#set_style_property "margin" "10px" ;
  details >>> add_common_option importance ;
  details >>> add_common_option progress ;
  details >>> add_common_option length ;
  details >>> add_common_option deadline_t ;
(*  ( details#obj >>> JSOO.call_method "normalize" [| |] >>> ignore ) ; *)

  (* message *)
  let message_content =
    let block = (*TODO: catch error on 1xx, 3xx, 4xx, 5xx*)
      AXOCom.dynload_post "./" 
        [
          ("__eliom_na__name", "ocsforge_get_message") ;
          ("task", string_of_int t.id) ;
        ]
        AXOCom.parse_xml
    in
    AXOCom.check_for_error block ;
    block >>> JSOO.get "documentElement"
  in
  let message = new AXOWidgets.common_wrap message_content in
  let message_box = new AXOToolkit.block_container in
(*  message_box#set_margin_left 10 ;
  message_box#set_style_property "border" "1px solid" ;
  message_box#set_position AXOWidgets.Relative ;
  message_box#set_background "white" ;
  message_box#set_style_property "margin" "10px" ; *)
  message_box#add_common message ;

  let title = AXOToolkit.text t.sub in

    form#set_style_property "-webkit-border-radius" "5px" ;
    form#set_style_property "-moz-border-radius" "5px" ;
    form#set_background "silver" ;
    form#add_common ( title            :> AXOWidgets.common ) ;
    form#add_common ( details          :> AXOWidgets.common ) ;
    form#add_common ( message_box      :> AXOWidgets.common ) ;

    popup





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
             editable = (o >>> get_attr "editable") >>> bool_of_string ;
              project = (o >>> get_attr "project") >>> bool_of_string ;
         } )
      (fun o ->
         if o >>> AXOJs.Node.n_children = 2
         then (o >>> AXOJs.Node.child 1) >>> AXOJs.Node.children
         else [] )
      (tree >>> JSOO.get "documentElement")
  in
    tree

let make_line t =
  (* The whole line container *)
  let main = new AXOToolkit.li_widget_container in

  (* The subject *)
  let subject = new AXOToolkit.inline_widget_text t.sub in

  (* The other columns *)
  let columns = new AXOToolkit.inline_container in
  let new_button = new AXOToolkit.img_button ~alt:"New subtask/subproject"
    "../../document-new-from-template.png" (*FIXME: have the server giving the dir argument*)
  in
  new_button#set_position AXOWidgets.Absolute ;
  new_button#set_x 2 ;
  let detail_button = new AXOToolkit.img_button ~alt:"Task details"
    "../../document-preview.png"
  in
  detail_button#set_position AXOWidgets.Absolute ;
  detail_button#set_x 20 ;

  new_button#add_click_action
    (fun () -> new_task_pop_up t.id
       (AXOWidgets.Absolute, subject#get_x, subject#get_y)
    ) ;
  detail_button#add_click_action
    (fun () -> let p = task_detail_pop_up t in
       p#set_position AXOWidgets.Absolute ;
       p#set_style_property "left" "10%" ;
       p#set_style_property "top" "10%" ;
       p#set_style_property "width" "80%" ;
       p#set_style_property "height" "80%" ;
       p#show
    ) ;

  (* The mash up *)
    main#set_style_property "listStyleType" "none" ;
    columns#add_common ( new_button    :> AXOWidgets.common ) ;
    columns#add_common ( detail_button :> AXOWidgets.common ) ;
    main#add_common    ( columns       :> AXOWidgets.common ) ;
    main#add_common    ( subject       :> AXOWidgets.common ) ;
    main

let main_tree root_task =
  let task_tree = get_task_tree root_task in
    AXOToolkit.foldable_tree ~depth:3 ~persistent_as_container:true task_tree
      (fun t l f ->
         ((if l = []
           then
             ( (new AXOToolkit.inline_text_widget_button ~activated:false ". "
               ) :> AXOWidgets.generic_button )
           else
             ( (if f
                then new AXOToolkit.cyclic_img_button "expand/collapse"
                  "../../arrow-right.png" [ "../../arrow-down.png" ]
                else new AXOToolkit.cyclic_img_button "expand/collapse"
                  "../../arrow-down.png" [ "../../arrow-right.png" ]

               ) :> AXOWidgets.generic_button )
          ),
          ((make_line t) :> AXOWidgets.generic_container),
          (let u = new AXOToolkit.ul_widget_container in
             u#set_style_property "marginTop" "0px" ;
             u#set_style_property "marginBottom" "0px" ;
             (u :> AXOWidgets.generic_container)
          )
         )
      
      )
      (new AXOToolkit.block_container)


let show_main task =
  let c = new AXOToolkit.ul_widget_container in

  let m = main_tree task in
  c#add_common ( m :> AXOWidgets.common ) ;
  c#set_style_property "marginLeft" "40px;" ;
  (c :> AXOWidgets.common)

let _ =
  if Regexp.test
       (Regexp.make "tasks")
       (AXOJs.Misc.get_location () >>> JSOO.get "pathname" >>> JSOO.as_string)
  then
    try 
      let div =
         AXOJs.Node.document >>> JSOO.call_method "getElementById"
                                   [| JSOO.string "ocsforge_task_tree" |]
      in
      let noscript = div >>> AXOJs.Node.child 0 in
      let container = new AXOToolkit.block_container in
      let reload () =
        container#wipe_content ;
        container#add_common
           (show_main
              (Scanf.sscanf
                 (noscript >>> AXOJs.Node.get_attribute "id")
                 "root_task_%li" (fun li -> li))
           )
      in
      let reload_button = new AXOToolkit.inline_text_button "RELOAD" in
      reload_button#add_click_action reload ;
      div >>> AXOJs.Node.append reload_button#obj ;
      div >>> AXOJs.Node.append container#obj ;
      reload ()
    with exc -> AXOJs.blunt_alert (Printexc.to_string exc)



