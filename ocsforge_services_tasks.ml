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

let ( ** ) = Eliom_parameters.prod
let ( >>= ) = Lwt.bind

module Params = Eliom_parameters
module Data = Ocsforge_data
module Types = Ocsforge_types
module Olang = Ocsforge_lang

open CalendarLib



let set_length_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_length"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string int_of_string)
          (Olang.string_of_t_opt string_of_int)
          "length")
      )
    (* error_handler *)
    (fun sp () (task, length) ->
       Data.edit_task ~sp ~task
         ~length:(Olang.apply_on_opted Calendar.Period.day length)
         () )

let set_progress_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_progress"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string Int32.of_string)
          (Olang.string_of_t_opt Int32.to_string)
          "progress")
      )
    (* error_handler *)
    (fun sp () (task, progress) ->
       Data.edit_task ~sp ~task ~progress ())

let set_importance_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_importance"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string Int32.of_string)
          (Olang.string_of_t_opt Int32.to_string)
          "importance")
      )
    (* error_handler *)
    (fun sp () (task, importance) ->
       Data.edit_task ~sp ~task ~importance ())

let set_deadline_time_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_deadline_t"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string int_of_string)
          (Olang.string_of_t_opt string_of_int)
          "deadline_t")
      )
    (* error_handler *)
    (fun sp () (task, dlt) ->
       Data.edit_task ~sp ~task
         ~deadline_time:(Olang.apply_on_opted
                           (Date.add (Date.today ()))
                           (Olang.apply_on_opted Date.Period.day dlt))
         ())

let set_deadline_version_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_deadline_v"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.string "deadline_v")
      )
    (* error_handler *)
    (fun sp () (task, dlv) ->
       Data.edit_task ~sp ~task
         ~deadline_version:(if dlv = "" then None else Some dlv) ())

let set_kind_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_kind"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (* error_handler *)
    (fun sp () (task, kind) ->
       Data.edit_task ~sp ~task ~kind ())

let new_task_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_add_task"
    ~options:`NoReload
    ~post_params:(
       ((Params.user_type Types.task_of_string Types.string_of_task "parent") **
        ((Params.string "subject") **
         ((Params.string "text") **
          ((Params.user_type
              (Olang.t_opt_of_string int_of_string)
              (Olang.string_of_t_opt string_of_int)
              "length") **
           ((Params.user_type
               (Olang.t_opt_of_string Int32.of_string)
               (Olang.string_of_t_opt Int32.to_string)
               "progress") **
            ((Params.user_type
                (Olang.t_opt_of_string Int32.of_string)
                (Olang.string_of_t_opt Int32.to_string)
                "importance") **
             ((Params.user_type
                 (Olang.t_opt_of_string int_of_string)
                 (Olang.string_of_t_opt string_of_int)
                 "deadline_t") **
              (((Params.user_type
                   (Olang.t_opt_of_string (fun k -> k))
                   (Olang.string_of_t_opt (fun k -> k))
                   "deadline_v") **
               ((Params.user_type
                   (Olang.t_opt_of_string (fun k -> k))
                   (Olang.string_of_t_opt (fun k -> k))
                   "kind")
               ))))))))))
    )
    (* error handler ? *)
    (fun sp () (parent,
                (subject,
                 (text,
                  (length,
                   (progress,
                    (importance,
                     (deadline_time,
                      (deadline_version,
                       kind         ))))))))
           ->
       let deadline_time = 
         Olang.apply_on_opted
           (Date.add (Date.today ()))
           (Olang.apply_on_opted Date.Period.day deadline_time)
       in
       let length = Olang.apply_on_opted Calendar.Period.day length in
       Data.new_task ~sp ~parent ~subject ~text
          ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
          () >>= fun _ -> Lwt.return ())


let register_dump_tree_service ?sp path tree_widget task_widget =
  Lwt.return (
  Eliom_duce.Xhtml.register_new_service
    ?sp
    ~path:[ path ; "tasks" ; "" ]
    ~get_params:(Params.opt (Params.int32 "id"))
    (fun sp id () ->
       let p_d = Wiki_widgets_interface.Page_displayable in
       let e404 = Wiki_widgets_interface.Page_404 in
       match id with

         | None ->
             begin
               Data.get_area_for_page ~sp ~page:path        >>= fun ri ->
               ((tree_widget#display ~sp
                   ~root_task:( Olang.unopt ri.Types.r_root_task )
                ) : {{ Xhtmltypes_duce.flows }} Lwt.t)
                                                            >>= fun content ->
              let gen_box _ = Lwt.return
                (None, content, p_d, Some "Ocsforge - task tree")
              in
              Ocsisite.wikibox_widget#display_container
                ~sp
                ~wiki:ri.Types.r_wiki
                ~menu_style:`Linear
                ~page:((Ocsigen_lib.string_of_url_path ~encode:true []), [])
                ~gen_box

              >>= fun (html, _) -> Lwt.return html
             end

         | Some id ->
             begin
               let task = Types.task_of_sql id in
               ((task_widget#display ~sp ~task
                ) : {{ Xhtmltypes_duce.flows }} Lwt.t)   >>= fun c ->
               Data.get_area_for_task ~sp ~task          >>= fun ri1 ->
               Data.get_area_for_page ~sp ~page:path     >>= fun ri2 ->
               let gen_box _ = Lwt.return
                 (None,
                  c,
                  (if ri1.Types.r_id = ri2.Types.r_id then p_d else e404),
                  Some "Ocsforge - task")
               in
               Ocsisite.wikibox_widget#display_container
                 ~sp
                 ~wiki:ri1.Types.r_wiki
                 ~menu_style:`Linear
                 ~page:((Ocsigen_lib.string_of_url_path ~encode:true []), [])
                 ~gen_box

               >>= fun r -> Lwt.return (fst r) 
             end
    )
  )

let register_get_message_service message_widget =
  Eliom_duce.Blocks.register_new_post_coservice'
    ~name:"ocsforge_get_message"
    ~post_params:(Params.int32 "task")
    (fun sp () task ->
       Data.get_task ~sp ~task:(Types.task_of_sql task) >>= fun t ->
       message_widget#display ~sp ?classes:(Some ["ocsforge_task_message"])
         ~data:t.Types.t_message
         ()
       >>= fun ( b : Xhtmltypes_duce.block ) ->
         Lwt.return {{ [ <div>[ {: b :} ] ] }}
    )

type supported_format =
  | Xml

let register_xml_dump_services tree_widget task_widget =
  Eliom_duce.Xml.register_new_post_coservice'
    ~name:"ocsforge_task_dump"
    ~post_params:(
      (  Params.int32 "root")
       **(  (Params.user_type
               ~of_string:(
                 fun s -> if s = "xml" then Xml else failwith "supported_format"
               )
               ~to_string:(fun Xml -> "xml")
               "format"
               )
          **(  (Params.opt (Params.int "depth")))
             **(Params.bool "with_deleted")
         )
    )
    (fun sp () (root, (fmt, (depth, with_deleted))) -> match fmt with
       | Xml ->
           begin
             let root = Types.task_of_sql root in
             Data.get_tree ~sp ~root ~with_deleted ?depth ()
                                                      >>= fun t ->
             Ocsforge_xml_tree_dump.xml_of_tree ~sp t >>= fun t ->
             Lwt.return (t : {{ Ocamlduce.Load.anyxml }})
           end
    ) ;
  Ocsforge_sql.get_projects_path_list () >>= fun ppl ->
  Lwt_util.iter_serial
    (fun p ->
       register_dump_tree_service p tree_widget task_widget
       >>= fun _ -> Lwt.return ()
    )
    ppl




let set_repository_path_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_repository_path"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.right_area_of_string
          Types.string_of_right_area
          "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "path")
      )
    (* error_handler *)
    (fun sp () (area, repository_path) ->
       Data.edit_area ~sp ~area ~repository_path ())

let set_repository_kind_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_repository_kind"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.right_area_of_string
          Types.string_of_right_area
          "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (* error_handler *)
    (fun sp () (area, repository_kind) ->
       Data.edit_area ~sp ~area ~repository_kind ())

let set_version_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_version"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.right_area_of_string
          Types.string_of_right_area
          "id") **
       (Params.string "version")
      )
    (* error_handler *)
    (fun sp () (area, version) ->
       Data.edit_area ~sp ~area ~version ())


let register_new_project_service tree_widget task_widget =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_add_project"
    ~options:`NoReload
    ~post_params:(
       ((Params.user_type
           Types.task_of_string Types.string_of_task
           "parent") **
        ((Params.string "name") **
          ((Params.user_type
              (Olang.t_opt_of_string int_of_string)
              (Olang.string_of_t_opt string_of_int)
              "length") **
           ((Params.user_type
               (Olang.t_opt_of_string Int32.of_string)
               (Olang.string_of_t_opt Int32.to_string)
               "progress") **
            ((Params.user_type
                (Olang.t_opt_of_string Int32.of_string)
                (Olang.string_of_t_opt Int32.to_string)
                "importance") **
             ((Params.user_type
                 (Olang.t_opt_of_string int_of_string)
                 (Olang.string_of_t_opt string_of_int)
                 "deadline_t") **
               ((Params.user_type
                   (Olang.t_opt_of_string (fun k -> k))
                   (Olang.string_of_t_opt (fun k -> k))
                   "kind") **
                ((Params.opt (Params.string "repo_kind")) **
                 (Params.opt (Params.string "repo_path"))
               ))))))))
    )
    (* error handler ? *)
    (fun sp () (parent,
                (name,
                  (length,
                   (progress,
                    (importance,
                     (deadline,
                      (kind,
                       (repository_kind,
                        repository_path))))))))
           ->
       let deadline = 
         Olang.apply_on_opted
           (Date.add (Date.today ()))
           (Olang.apply_on_opted Date.Period.day deadline)
       in
       let length = Olang.apply_on_opted Calendar.Period.day length in
       Data.new_project ~sp ~parent ~name
         ?length ?importance ?deadline ?kind
         ?repository_kind ?repository_path 
               ~wiki_container:Wiki.default_container_page
               ()                                               >>= fun task ->
       Data.get_area_for_task ~sp ~task                         >>= fun area ->
       Ocsforge_sql.get_project_path ~area:area.Types.r_id ()
       >>= (function
              | None -> Lwt.return ()
              | Some path ->
                  (
                   register_dump_tree_service ~sp path tree_widget task_widget
                     >>= fun _ ->
                   Ocsforge_services_source.register_repository_service ~sp path
                     >>= fun _ ->
                   Lwt.return ()
                  )
           )
    )

