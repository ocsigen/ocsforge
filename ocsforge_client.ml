open AXOLang;;
let ( >>> ) x f = f x;;
let get_path () =
  let s =
    ((AXOJs.Misc.get_location ()) >>> (JSOO.get "pathname")) >>> JSOO.
      as_string
  in
    try (Regexp.exec (Regexp.make "/(.*)/sources.*") s).(1)
    with | exc -> (AXOJs.alert (Printexc.to_string exc); raise exc);;
let get_version () =
  let s =
    ((AXOJs.Misc.get_location ()) >>> (JSOO.get "search")) >>> JSOO.as_string
  in
    try
      if Regexp.test (Regexp.make ".*version=.*") s
      then Some (Regexp.exec (Regexp.make "version=([^&]*)") s).(1)
      else None
    with | exc -> (AXOJs.alert (Printexc.to_string exc); raise exc);;
let get_trs dir =
  let l =
    AXOCom.dynload_post "./"
      (("__eliom_na__name", "ocsforge_repository_tree") ::
        ("dir",
         ((get_path ()) ^
            ("/" ^ (String.sub dir 1 (pred (String.length dir)))))) ::
        (match get_version () with
         | None -> []
         | Some s -> [ ("version", s) ]))
      AXOCom.parse_xml
  in
    (AXOCom.check_for_error l;
     (l >>> (JSOO.get "documentElement")) >>> AXOJs.Node.children);;
type task =
  { id : int; sub : string; length : int option; progress : int option;
    importance : int option; kind : string; editable : bool; project : bool
  };;
type separator = { sid : int; scontent : string; safter : int };;
let rec new_task_pop_up id (pos, x, y) =
  let form = new AXOToolkit.vbox in
  let popup = new AXOToolkit.popup form in
  let details = new AXOToolkit.block_widget_container in
  let more =
    new AXOToolkit.block_foldable
      (new AXOToolkit.cyclic_inline_text_button "More options"
         [ "Less options" ] :>
        AXOWidgets.generic_button)
      new AXOToolkit.inline_container
      (details :> AXOWidgets.generic_container) in
  let importance_select =
    new AXOToolkit.select (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string) None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump: 5 ~min: 0 ~max: 100 ())) in
  let progress_select =
    new AXOToolkit.select (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string) None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump: 5 ~min: 0 ~max: 100 ())) in
  let length_select =
    new AXOToolkit.select (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string) None
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump: 6 ~min: 0 ~max: 48 ())) in
  let kind_input = new AXOToolkit.text_input ""
  in
    (more#set_margin_left 10;
     details#set_style_property "border" "1px solid";
     details#add_common (AXOToolkit.text "importance : ");
     details#add_common (importance_select :> AXOWidgets.common);
     details#add_common new AXOToolkit.br;
     details#add_common (AXOToolkit.text "progress : ");
     details#add_common (progress_select :> AXOWidgets.common);
     details#add_common new AXOToolkit.br;
     details#add_common (AXOToolkit.text "length : ");
     details#add_common (length_select :> AXOWidgets.common);
     details#add_common (AXOToolkit.text " days");
     details#add_common new AXOToolkit.br;
     details#add_common (AXOToolkit.text "category : ");
     details#add_common (kind_input :> AXOWidgets.common);
     let title_input = new AXOToolkit.text_input ""
     in
       (title_input#set_attribute "size" "100";
        let save_button = new AXOToolkit.inline_text_widget_button "SAVE"
        in
          (save_button#set_margin_left 70;
           save_button#add_click_action
             (fun () ->
                (popup#hide;
                 let (c, m) =
                   AXOCom.http_post "./"
                     [ ("__eliom_na__name", "ocsforge_add_task");
                       ("parent", (string_of_int id));
                       ("subject", (title_input#get_value)); ("text", "");
                       ("length",
                        (LOption.string_of_t_opt string_of_int
                           length_select#get_value));
                       ("progress",
                        (LOption.string_of_t_opt string_of_int
                           progress_select#get_value));
                       ("importance",
                        (LOption.string_of_t_opt string_of_int
                           importance_select#get_value));
                       ("kind", (kind_input#get_value)) ]
                 in
                   AXOCom.alert_on_code
                     ~on_2xx:
                       (fun _ -> AXOJs.alert "task successfully added")
                     ~on_4xx:
                       (fun (_, m) ->
                          AXOJs.rich_alert
                            ((AXOCom.parse_xml m) >>>
                               (JSOO.get "documentElement")))
                     ~on_5xx:
                       (fun (_, m) ->
                          AXOJs.rich_alert
                            ((AXOCom.parse_xml m) >>>
                               (JSOO.get "documentElement")))
                     (c, m)));
           let project_button =
             new AXOToolkit.inline_text_widget_button "NEW PROJECT"
           in
             (project_button#add_click_action
                (fun () -> (popup#hide; new_project_popup id (pos, x, y)));
              form#add_common (title_input :> AXOWidgets.common);
              form#add_common (more :> AXOWidgets.common);
              form#add_common
                (let span = new AXOToolkit.inline_container
                 in
                   (span#add_common (project_button :> AXOWidgets.common);
                    span#add_common (save_button :> AXOWidgets.common);
                    (span :> AXOWidgets.common)));
              popup#set_position pos;
              popup#set_x x;
              popup#set_y y;
              popup#show))))
and new_project_popup id (pos, x, y) =
  let form = new AXOToolkit.vbox in
  let popup = new AXOToolkit.popup form in
  let title_input = new AXOToolkit.text_input ""
  in
    (title_input#set_attribute "size" "50";
     let repo_kind_select =
       new AXOToolkit.select (LOption.string_of_t_opt (fun k -> k))
         (LOption.t_opt_of_string (fun k -> k)) None
         [ Some "Darcs"; Some "SVN" ] in
     let repo_path_input = new AXOToolkit.text_input "" in
     let save_button = new AXOToolkit.inline_text_widget_button "SAVE"
     in
       (save_button#set_margin_left 70;
        save_button#add_click_action
          (fun () ->
             (popup#hide;
              let (c, m) =
                AXOCom.http_post "./"
                  (LOption.optionnaly_add_to_list
                     (LOption.optionnaly_add_to_list
                        [ ("__eliom_na__name", "ocsforge_add_project");
                          ("parent", (string_of_int id));
                          ("name", (title_input#get_value)); ("length", "");
                          ("progress", ""); ("importance", ""); ("kind", "") ]
                        (LOption.apply_on_opted (fun s -> ("repo_kind", s))
                           repo_kind_select#get_value))
                     (LOption.apply_on_opted (fun s -> ("repo_path", s))
                        (LOption.t_opt_of_string (fun k -> k)
                           repo_path_input#get_value)))
              in
                AXOCom.alert_on_code
                  ~on_2xx:
                    (fun _ -> AXOJs.alert "project successfully added")
                  ~on_4xx:
                    (fun (_, m) ->
                       AXOJs.rich_alert
                         ((AXOCom.parse_xml m) >>>
                            (JSOO.get "documentElement")))
                  ~on_5xx:
                    (fun (_, m) ->
                       AXOJs.rich_alert
                         ((AXOCom.parse_xml m) >>>
                            (JSOO.get "documentElement")))
                  (c, m)));
        let task_button = new AXOToolkit.inline_text_widget_button "NEW TASK"
        in
          (task_button#add_click_action
             (fun () -> (popup#hide; new_task_pop_up id (pos, x, y)));
           form#add_common (title_input :> AXOWidgets.common);
           form#add_common
             (let span = new AXOToolkit.inline_container
              in
                (span#add_common (AXOToolkit.text "Repository kind : ");
                 span#add_common (repo_kind_select :> AXOWidgets.common);
                 (span :> AXOWidgets.common)));
           form#add_common
             (let span = new AXOToolkit.inline_container
              in
                (span#add_common (AXOToolkit.text "Repository path : ");
                 span#add_common (repo_path_input :> AXOWidgets.common);
                 (span :> AXOWidgets.common)));
           form#add_common new AXOToolkit.br;
           form#add_common
             (let span = new AXOToolkit.inline_container
              in
                (span#add_common (task_button :> AXOWidgets.common);
                 span#add_common (save_button :> AXOWidgets.common);
                 (span :> AXOWidgets.common)));
           popup#set_position pos;
           popup#set_x x;
           popup#set_y y;
           popup#show)));;
let tree_of_dom_tree get_content get_children dt =
  let rec aux dt =
    {
      LTree.content = get_content dt;
      LTree.children = List.map aux (get_children dt);
    }
  in aux dt;;
let get_task_tree root_task =
  let info =
    AXOCom.dynload_post "./"
      [ ("__eliom_na__name", "ocsforge_task_dump");
        ("root", (Int32.to_string root_task)); ("format", "xml") ]
      AXOCom.parse_xml in
  let get_attr n o =
    (((o >>> (JSOO.get "attributes")) >>> (JSOO.get n)) >>>
       (JSOO.get "value"))
      >>> JSOO.as_string
  in
    (AXOCom.check_for_error info;
     let tree =
       tree_of_dom_tree
         (fun o ->
            let sub_ =
              ((o >>> (AXOJs.Node.child 0)) >>> (JSOO.get "textContent")) >>>
                JSOO.as_string
            in
              {
                sub = sub_;
                progress =
                  (o >>> (get_attr "progress")) >>>
                    (LOption.t_opt_of_string int_of_string);
                importance =
                  (o >>> (get_attr "importance")) >>>
                    (LOption.t_opt_of_string int_of_string);
                kind = o >>> (get_attr "kind");
                length =
                  (o >>> (get_attr "_length_")) >>>
                    (LOption.t_opt_of_string int_of_string);
                id = (o >>> (get_attr "id")) >>> int_of_string;
                editable = (o >>> (get_attr "editable")) >>> bool_of_string;
                project = (o >>> (get_attr "project")) >>> bool_of_string;
              })
         (fun o ->
            if (o >>> AXOJs.Node.n_children) = 2
            then (o >>> (AXOJs.Node.child 1)) >>> AXOJs.Node.children
            else [])
         ((info >>> (JSOO.get "documentElement")) >>> (AXOJs.Node.child 1)) in
     let seps =
       (((info >>> (JSOO.get "documentElement")) >>> (AXOJs.Node.child 0))
          >>> AXOJs.Node.children)
         >>>
         (List.map
            (fun o ->
               {
                 sid = (o >>> (get_attr "id")) >>> int_of_string;
                 scontent =
                   (o >>> (JSOO.get "textContent")) >>> JSOO.as_string;
                 safter = (o >>> (get_attr "after")) >>> int_of_string;
               }))
     in (tree, seps));;
let make_line t =
  let main = new AXOToolkit.li_widget_container in
  let subject = new AXOToolkit.inline_widget_text t.sub in
  let columns = new AXOToolkit.inline_container in
  let new_button =
    new AXOToolkit.img_button ~alt: "New subtask/subproject"
      "../../document-new-from-template.png"
  in
    (new_button#set_position AXOWidgets.Absolute;
     new_button#set_x 2;
     let details_button =
       new AXOToolkit.img_link ~href: ("?id=" ^ (string_of_int t.id))
         ~src: "../../document-preview.png" ~alt: "task details"
     in
       (details_button#set_position AXOWidgets.Absolute;
        details_button#set_x 20;
        new_button#add_click_action
          (fun () ->
             new_task_pop_up t.id
               (AXOWidgets.Absolute, (subject#get_x), (subject#get_y)));
        main#set_style_property "listStyleType" "none";
        columns#add_common (new_button :> AXOWidgets.common);
        columns#add_common (details_button :> AXOWidgets.common);
        main#add_common (columns :> AXOWidgets.common);
        main#add_common (subject :> AXOWidgets.common);
        main));;
let main_tree root_task =
  let (task_tree, _) = get_task_tree root_task
  in
    AXOToolkit.foldable_tree ~depth: 3 ~persistent_as_container: true
      task_tree
      (fun t l f ->
         ((if l = []
           then
             (new AXOToolkit.inline_text_widget_button ~activated: false ". " :>
               AXOWidgets.generic_button)
           else
             (if f
              then
                new AXOToolkit.cyclic_img_button "expand/collapse"
                  "../../arrow-right.png" [ "../../arrow-down.png" ]
              else
                new AXOToolkit.cyclic_img_button "expand/collapse"
                  "../../arrow-down.png" [ "../../arrow-right.png" ] :>
               AXOWidgets.generic_button)),
          (make_line t :> AXOWidgets.generic_container),
          (let u = new AXOToolkit.ul_widget_container
           in
             (u#set_style_property "marginTop" "0px";
              u#set_style_property "marginBottom" "0px";
              (u :> AXOWidgets.generic_container)))))
      new AXOToolkit.block_container;;
let show_main task =
  let c = new AXOToolkit.ul_widget_container in
  let m = main_tree task
  in
    (c#add_common (m :> AXOWidgets.common);
     c#set_style_property "marginLeft" "40px;";
     (c :> AXOWidgets.common));;
let _ =
  if
    Regexp.test (Regexp.make "tasks/?$")
      (((AXOJs.Misc.get_location ()) >>> (JSOO.get "pathname")) >>> JSOO.
         as_string)
  then
    (try
       let div =
         AXOJs.Node.document >>>
           (JSOO.call_method "getElementById"
              [| JSOO.string "ocsforge_task_tree" |]) in
       let noscript = div >>> (AXOJs.Node.child 0) in
       let container = new AXOToolkit.block_container in
       let reload () =
         (container#wipe_content;
          container#add_common
            (show_main
               (Scanf.sscanf (noscript >>> (AXOJs.Node.get_attribute "id"))
                  "root_task_%li" (fun li -> li)))) in
       let reload_button = new AXOToolkit.inline_text_button "RELOAD"
       in
         (reload_button#add_click_action reload;
          div >>> (AXOJs.Node.append reload_button#obj);
          div >>> (AXOJs.Node.append container#obj);
          reload ())
     with | exc -> AXOJs.blunt_alert (Printexc.to_string exc))
  else ();;
let _ =
  Eliom_obrowser_client.register_closure 0x502921C6
    (fun ((name, id, service) : (string * int32 * string)) ->
       AXOCom.http_post "./"
         [ ("__eliom_na__name", service); ("id", (Int32.to_string id));
           (name,
            (((AXOJs.Node.document >>>
                 (JSOO.call_method "getElementById"
                    [| JSOO.string (name ^ (Int32.to_string id)) |]))
                >>> (JSOO.get "value"))
               >>> JSOO.as_string)) ]);;
