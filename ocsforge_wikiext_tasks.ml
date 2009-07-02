(* Ocsimore
 * Copyright (C) 2009
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
module Params = Eliom_parameters
module Types = Ocsforge_types
module Services = Ocsforge_services_tasks

(**Functions to pass CSS information *)
(* Functions to add a CSS header for wiki, and to query whether
*    this header must be added *)
let add_wiki_css_header, must_add_wiki_css_header =
  let key = Polytables.make_key () in
    (fun sp -> Polytables.set ~table:(Eliom_sessions.get_request_cache sp)
                 ~key ~value:true),
    (fun sp ->
       try Polytables.get ~table:(Eliom_sessions.get_request_cache sp) ~key
       with Not_found -> false)

(* We define the function generating this header there *)
let send_css_up css_file sp =
  add_wiki_css_header sp ;
  Ocsimore_page.add_html_header_hook
    (fun sp ->
       if must_add_wiki_css_header sp then
         {{ [ {: Eliom_duce.Xhtml.css_link
            (Ocsimore_page.static_file_uri sp [ css_file ]) () :}
         ] }}
       else {{ [] }}
    )





let register_wikiext wp
      (tree_widget : Ocsforge_widgets_tasks.tree_widget)
      nl_service
      repo_service
     =

  Wiki_syntax.add_extension
    ~wp ~name:"ocsforge_tree" ~wiki_content:false
    ((fun bi args content ->

      Wikicreole.Block
        (Lwt.catch
           (fun () ->
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let (nl_id, nl_by, nl_dsc) = (*TODO: clean this messy part*)
              let tmp = Params.get_non_localized_get_parameters sp
                          Services.nl_param
              in match tmp with
                | Some (id, (by, dsc)) ->
                    (Some id, Some by, Some dsc)
                | None -> (None, None, None)
            in
            let we_id = Ocsforge_types.task_of_string (List.assoc "id" args) in
            let sort =
              let warp getter =
                (fun t1 t2 -> Ocsforge_lang.compare_opt (getter t1) (getter t2))
              in match nl_by with
                | Some "length" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_length
                                  | Types.Tree.Nil ->
                                      None))
                | Some "progress" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_progress
                                  | Types.Tree.Nil ->
                                      None))
                | Some "importance" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_importance
                                  | Types.Tree.Nil ->
                                      None))
                | Some "kind" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_kind
                                  | Types.Tree.Nil ->
                                      None))
                | Some "deadline_time" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_deadline_time
                                  | Types.Tree.Nil ->
                                      None))
                | Some "deadline_version" ->
                    Some (warp (function
                                  | Types.Tree.Node (t, _) ->
                                      t.Types.t_deadline_version
                                  | Types.Tree.Nil ->
                                      None))
                | None | Some _ -> None
            in
            let sort =
              if (Ocsforge_lang.unopt ~default:false nl_dsc)
              then Ocsforge_lang.apply_on_opted
                     (fun f -> (fun t2 t1 -> f t1 t2))
                     sort
              else sort
            in
            let fields =
              let f = Ocsforge_lang.assoc_all "field" args in
                if f = []
                then ["importance"]
                else f
            in
            let () =  send_css_up "ocsforge_tree.css" sp
            in
            tree_widget#display ~sp
              ~root_tasks:(Ocsforge_lang.unopt ~default:we_id nl_id, we_id)
              ~fields
              ~nl_service
	      ~repo_service
              ?sort ()
                                   >>= fun b ->
              Lwt.return {{  b }}
           )

           (function
              | Not_found | Failure _ -> 
                  let s = Wiki_syntax.string_of_extension "raw" args content in
                  Lwt.return {{ [ <b>{: s :} ] }}
              | exc ->
                  let s = Wiki_syntax.string_of_extension "raw" args content in
                  Lwt.return {{ [ <b>[ !{: s :} <br>[]
                                  !{: Printexc.to_string exc :} ] ] }})
        )

    ) : Wiki_syntax.syntax_extension ) ;


(*
  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_new_task" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let parent = (Types.task_of_string (List.assoc "parent" args)) in
            add_task_widget#display ~sp ?classes ~parent ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;

  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_task" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let id = Types.task_of_string (List.assoc "id" args) in
            Roles.get_area_role ~sp id >>= fun role ->
            task_widget#display ~sp ?classes id ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;


  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_tasks" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let ids =
              List.map Types.right_area_of_string (Lang.assoc_all "id" args)
            in
            let params = List.map (Lang.assoc_all "param" args)
            in
            Roles.get_area_role ~sp id >>= fun role ->
            tree_widget#display ~sp ?classes ids params ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;
 *)

