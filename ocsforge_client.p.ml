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

(** @author Raphael Proust *)

begin.client

  open AXOLang
  let (>>>) x f = f x
  let (@@) f g = fun x -> f (g x)

end


begin.client (* For source browsing (FIXME:raise a server exception... will probably get type checked with Eliom services*)

  let get_path () =
    let pl = AXOCom.Url.get_path () in
    let rec del_trainling_sources accu = function
      | [] -> raise Not_found
      | [ "sources" ] | [ "sources" ; "" ] -> List.rev accu
      | hd :: tl -> del_trainling_sources (hd :: accu) tl
    in
    del_trainling_sources [] pl

  let get_version () =
    try Some (AXOCom.Url.get_argument "version") with Not_found -> None

  let get_trs dir =
    let l = (* errors on 1xx, 3xx, 4xx and 5xx are catched later on*)
      AXOCom.dynload_post "./"
        (   ("__eliom_na__name", "ocsforge_repository_tree")
         :: ("dir", String.concat "/"
                    ( (try get_path () with Not_found -> [])
                     @ [ (String.sub dir 1 (pred (String.length dir))) ])
            )
         :: (match get_version () with | None -> [] | Some s -> [ ("version", s) ])
        )
        AXOCom.parse_xml
    in
    AXOCom.check_for_error l ; l >>> JSOO.get "documentElement" >>> AXOJs.Node.children

(* 
  let _ = (*TODO: don't use URL to check if the element should be launched. Have the page generation set a flag. *)
    if List.mem "sources" (AXOCom.Url.get_path ())
    then
    try
      begin
        let tbodies =
           (AXOJs.Node.body >>> JSOO.call_method "getElementsByTagName"
                                  [| AXOJs.string "tbody" |])
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
                                     [| AXOJs.string "img" |]
                      in
                      let trs =
                        lazy (
                          get_trs (folder >>> JSOO.get "textContent"
                                          >>> JSOO.as_string)
                        )
                      in
                      let click img () =
                        List.iter
                          (fun t -> tbody >>> AXOJs.Node.insert_before
                                      t (folder >>> JSOO.get "nextSibling"))
                          (Lazy.force trs) ;
                        img >>> AXOEvents.Onclick.clear ()
                      in
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

end


begin.client (* for task management and BTS *)

  (* internal representation of task *)
  type task =
    (*As there's no Calendar lib aviable length is in days *)
      { id : int                 ; sub : string           ;
        length : int option      ; progress : int option  ;
        importance : int option  ; kind : string          ;
        editable : bool ; project : bool ; movable : bool ;

      }
  (* internal representation of separator *)
  type separator = { sid : int ; scontent : string ; safter : int }

  (* poping up a "new task" pseudo-form (TODO: switch to Eliom client side form when aviable) *)
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

    (* populating details and fixing some visual effects *)
    more#set_style_property "marginLeft" (AXOWidgets.px_string 10) ;
    details#set_style_property "border" "1px solid" ;
    details#add_common ( AXOToolkit.text "importance : "        ) ;
    details#add_common ( importance_select :> AXOWidgets.common ) ;
    details#add_common ( new AXOToolkit.br ) ;
    details#add_common ( AXOToolkit.text "progress : "        ) ;
    details#add_common ( progress_select :> AXOWidgets.common ) ;
    details#add_common ( AXOToolkit.text " %"                 );

    let title_input = new AXOToolkit.text_input "" in
      title_input#set_attribute "size" "100" ;
    let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
      save_button#set_style_property "marginLeft" (AXOWidgets.px_string 70) ;
      save_button#add_click_action
        (fun () ->
           popup#hide ;
           let (c,m) =
             AXOCom.http_post "./"
                [ ("__eliom_na__name","ocsforge_add_task") ;
                  ("parent", string_of_int id            ) ;
                  ("subject", title_input#get_value      ) ;
                  ("text", "" ) ;

                  ("length", "" ) ;
                  ("progress",
                   LOption.string_of_t_opt string_of_int
                     progress_select#get_value              ) ;
                  ("importance",
                   LOption.string_of_t_opt string_of_int
                     importance_select#get_value            ) ;
                  ("kind", "" ) ;

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

      popup#set_style_property "position" (AXOWidgets.string_of_position pos) ;
      popup#set_style_property "left" (AXOWidgets.px_string x) ;
      popup#set_style_property "top" (AXOWidgets.px_string y) ;
      popup#show

  (* TODO: change this pseudo-form to a Eliom form. *)
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
      save_button#set_style_property "marginLeft" (AXOWidgets.px_string 70) ;
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
                        ("importance","") ;
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
      form#add_common (let span = new AXOToolkit.inline_container in
                        span#add_common (AXOToolkit.text "Repository kind : ") ;
                        span#add_common (repo_kind_select :> AXOWidgets.common);
                        (span :> AXOWidgets.common)
                      ) ;
      form#add_common (let span = new AXOToolkit.inline_container in
                        span#add_common (AXOToolkit.text "Repository path : ") ;
                        span#add_common (repo_path_input :> AXOWidgets.common) ;
                        (span :> AXOWidgets.common)
                      ) ;
      form#add_common ( new AXOToolkit.br ) ;
      form#add_common (let span = new AXOToolkit.inline_container in
                        span#add_common (task_button:> AXOWidgets.common) ;
                        span#add_common (save_button   :> AXOWidgets.common) ;
                        (span :> AXOWidgets.common)
      ) ;

      popup#set_style_property "position" (AXOWidgets.string_of_position pos) ;
      popup#set_style_property "left" (AXOWidgets.px_string x) ;
      popup#set_style_property "top" (AXOWidgets.px_string y) ;
      popup#show

  (* a function to automatically build a AXOLang.LTree.tree from an obj. *)
  let tree_of_dom_tree get_content get_children dt =
    let rec aux dt =
      {
        LTree.content  = get_content  dt ;
        LTree.children = List.map aux (get_children dt) ;
      }
    in
      aux dt

  (* make a XMLHttpRequest, parse it, and rebuild the tree. *)
  let get_task_tree root_task =
    let info = (*TODO: catch error on 1xx, 3xx, 4xx, 5xx*)
      AXOCom.dynload_post "./"
        [
          ("__eliom_na__name", "ocsforge_task_dump") ;
          ("root", Int32.to_string root_task) ;
          ("format", "xml") ;
          (* ("depth", "1") ; *)
        ]
        AXOCom.parse_xml
    in
    let get_attr n o = (* an alias for a long method chain *)
      (* /!\ DO NOT USE AXOJs.Node.get_attribute as it only works with (X)HTML nodes *)
      o >>> JSOO.get "attributes"
        >>> JSOO.get n
        >>> JSOO.get "value"
        >>> JSOO.as_string
    in
    AXOCom.check_for_error info ;

    let tree =
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
                   kind = (o >>> get_attr "kind"      ) ;
                 length = (o >>> get_attr "_length_" )
                             >>> LOption.t_opt_of_string int_of_string   ;
                     id = (o >>> get_attr "id" ) >>> int_of_string ;
               editable = (o >>> get_attr "editable") >>> bool_of_string ;
                project = (o >>> get_attr "project") >>> bool_of_string ;
                movable = (o >>> get_attr "movable") >>> bool_of_string ;
           } )
        (fun o ->
           if o >>> AXOJs.Node.n_children = 2
           then (o >>> AXOJs.Node.child 1) >>> AXOJs.Node.children
           else [] )
        (info >>> JSOO.get "documentElement" >>> AXOJs.Node.child 1)
    in
    let seps =
      (info >>> JSOO.get "documentElement" >>> AXOJs.Node.child 0 >>> AXOJs.Node.children)
        >>> List.map
         (fun o ->
            {      sid = o >>> get_attr "id" >>> int_of_string ;
              scontent = o >>> JSOO.get "textContent" >>> JSOO.as_string ;
                safter = o >>> get_attr "after" >>> int_of_string ;
            } )
    in
      (tree, seps)


  let make_separator s =
    let w = new AXOToolkit.li_widget_container in
    w#add_common (AXOToolkit.text s.scontent) ;
    w#set_attribute "id" ("separator" ^ string_of_int s.sid) ;
    w#set_style_property "border" "2px solid #FF0000" ;
    w#set_style_property "background" "#FF6666" ;
    (w :> AXOWidgets.common)


  let make_line t =
    (* The whole line container *)
    let main = new AXOToolkit.li_widget_container in

    (* The subject *)
    let subject =
      if t.project
      then ( (new AXOToolkit.link_widget ~href:("../" ^ t.sub ^ "/tasks/") t.sub
             ) :> AXOWidgets.generic_widget )
      else ( (new AXOToolkit.inline_widget_text t.sub
             ) :> AXOWidgets.generic_widget )
    in

    (* The other columns *)
    let columns = new AXOToolkit.inline_container in
    let new_button = new AXOToolkit.img_button ~alt:"New subtask/subproject"
      "/document-new-from-template.png" (*FIXME: use the static service in Eliom to have this automatically generated*)
    in
    new_button#set_style_property "position"
         (AXOWidgets.string_of_position AXOWidgets.Absolute) ;
    new_button#set_style_property "left" (AXOWidgets.px_string 2) ;

    let details_button = new AXOToolkit.img_link
      ~href:("?id=" ^ string_of_int t.id)
      ~src:"/document-preview.png" ~alt:"task details"
    in
    details_button#set_style_property "position"
         (AXOWidgets.string_of_position AXOWidgets.Absolute) ;
    details_button#set_style_property "left" (AXOWidgets.px_string 20) ;


    new_button#add_click_action
      (fun () -> new_task_pop_up t.id
         (AXOWidgets.Absolute,
          subject#obj >>> JSOO.get "offsetLeft" >>> JSOO.as_int ,
          subject#obj >>> JSOO.get "offsetTop" >>> JSOO.as_int )
      ) ;

    (* The mash up *)
      main#set_style_property "listStyleType" "none" ;
      main#set_attribute "id" ("ocsforge_task_" ^ string_of_int t.id) ;
      columns#add_common ( new_button     :> AXOWidgets.common ) ;
      columns#add_common ( details_button :> AXOWidgets.common ) ;
      main#add_common    ( columns        :> AXOWidgets.common ) ;
      main#add_common    ( subject        :> AXOWidgets.common ) ;
      main

  let main_tree root_task =
    let (task_tree, seps) = get_task_tree root_task in
      AXOToolkit.foldable_tree ~depth:1 ~persistent_as_container:true
        ~separators:( fun t _ _ ->
           LOption.apply_on_opted make_separator
              (LList.find_opt (fun s -> s.sid = t.id) seps)
        )
        (LTree.sort
           ~comp:( fun t1 t2 -> compare
                     t2.LTree.content.project
                     t1.LTree.content.project
           )
           task_tree
        )
        (fun t l f ->
           (if l = []
            then
              ( (new AXOToolkit.inline_text_widget_button ~activated:false ". "
                ) :> AXOWidgets.generic_button )
            else
              ( (if f
                 then new AXOToolkit.cyclic_img_button "expand/collapse"
                   "/arrow-right.png" [ "/arrow-down.png" ]
                 else new AXOToolkit.cyclic_img_button "expand/collapse"
                   "/arrow-down.png" [ "/arrow-right.png" ]

                ) :> AXOWidgets.generic_button )
            ),
           ((make_line t) :> AXOWidgets.generic_container),
           (let u = new AXOToolkit.ul_widget_container in
               u#set_style_property "marginTop" "0px" ;
               u#set_style_property "marginBottom" "0px" ;
               (u :> AXOWidgets.generic_container)
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
    if List.mem "tasks" (AXOCom.Url.get_path ())
       && (try let _ = AXOCom.Url.get_argument "id" in false
           with Not_found -> true)
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
        let reload_button = new AXOToolkit.inline_text_widget_button "RELOAD" in
        reload_button#add_click_action reload ;
        div >>> AXOJs.Node.append reload_button#obj ;
        div >>> AXOJs.Node.append container#obj ;
        reload ()
      with exc -> AXOJs.blunt_alert (Printexc.to_string exc)

end


(* for auto updating fields *)
let keep_up_to_date = (*TODO: use Eliom services instead*)
  (fun.client (name : string) (id : int32) (service : string) (salt : string) ->
     AXOCom.http_post "./"
      [
        "__eliom_na__name", service ;
        "id", Int32.to_string id ;
        name,  AXOJs.Node.document
                    >>> JSOO.call_method "getElementById"
                           [| JSOO.string (salt ^ name ^ Int32.to_string id) |] 
                    >>> JSOO.get "value"   
                    >>> JSOO.as_string
      ]
  )

