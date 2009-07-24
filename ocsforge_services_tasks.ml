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

open CalendarLib



let set_length_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_length"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Ocsforge_lang.period_of_string)
          (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_period)
          "length")
      )
    (* error_handler *)
    (fun sp () (task, length) ->
       Data.edit_task ~sp ~task ~length () )

let set_progress_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_progress"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Int32.of_string)
          (Ocsforge_lang.string_of_t_opt Int32.to_string)
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
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Int32.of_string)
          (Ocsforge_lang.string_of_t_opt Int32.to_string)
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
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Ocsforge_lang.date_of_string)
          (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_date)
          "deadline_t")
      )
    (* error_handler *)
    (fun sp () (task, deadline_time) ->
       Data.edit_task ~sp ~task ~deadline_time ())

let set_deadline_version_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_set_deadline_v"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
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
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string (fun s -> s))
          (Ocsforge_lang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (* error_handler *)
    (fun sp () (task, kind) ->
       Data.edit_task ~sp ~task ~kind ())


let new_task_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_add_task"
    ~options:`Reload
    ~post_params:(
       ((Params.user_type
           Types.task_of_string Types.string_of_task
           "parent") **
        ((Params.string "subject") **
         ((Params.string "text") **
          ((Params.user_type
              (Ocsforge_lang.t_opt_of_string Ocsforge_lang.period_of_string)
              (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_period)
              "length") **
           ((Params.user_type
               (Ocsforge_lang.t_opt_of_string Int32.of_string)
               (Ocsforge_lang.string_of_t_opt Int32.to_string)
               "progress") **
            ((Params.user_type
                (Ocsforge_lang.t_opt_of_string Int32.of_string)
                (Ocsforge_lang.string_of_t_opt Int32.to_string)
                "importance") **
             ((Params.user_type
                  (Ocsforge_lang.t_opt_of_string Ocsforge_lang.date_of_string)
                  (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_date)
                  "deadline_t") **
              (((Params.user_type
                   (Ocsforge_lang.t_opt_of_string (fun k -> k))
                   (Ocsforge_lang.string_of_t_opt (fun k -> k))
                   "deadline_v") **
               ((Params.user_type
                   (Ocsforge_lang.t_opt_of_string (fun k -> k))
                   (Ocsforge_lang.string_of_t_opt (fun k -> k))
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
       Data.new_task ~sp ~parent ~subject ~text
          ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
          () >>= fun _ -> Lwt.return ())


let register_dump_tree_service path root il_widget task_widget =
  Eliom_predefmod.Any.register_new_service
    ~path:[ path ; Types.string_of_task root ; "" ]
    ~get_params:Params.unit
    (fun sp _ _ ->
       ((task_widget#display ~sp ~root_task:root il_widget) : {{ Xhtmltypes_duce.flows }} Lwt.t) >>= fun content ->
       Ocsforge_data.get_area_for_task ~sp ~task:root    >>= fun ri ->
       let gen_box _ =
         Lwt.return
           (None,
            content,
            Wiki_widgets_interface.Page_displayable,
            Some "Ocsforge - task tree")
       in
         Ocsisite.wikibox_widget#display_container
           ~sp
           ~wiki:ri.Types.r_wiki
           ~menu_style:`Linear
           ~page:((Ocsigen_lib.string_of_url_path ~encode:true []), [])
           ~gen_box:gen_box
         >>= fun (html, code) -> Eliom_duce.Xhtml.send ~sp ~code html)

let register_xml_dump_services il_widget t_widget =
  Eliom_duce.Xml.register_new_post_coservice'
    ~name:"ocsforge_task_dump"
    ~post_params:(
      (  Params.int32 "root")
       **(  (Params.regexp(*FIXME: give the real needed function*)
                (Netstring_pcre.regexp "(xml)") "$1" (fun s -> s) "format")
          **(  (Params.opt (Params.int "depth")))
             **(Params.bool "with_deleted")
         )
    )
    (fun sp () (root, (fmt, (depth, with_deleted))) -> match fmt with
       | "xml" ->
           begin
             let root = Types.task_of_sql root in
             Ocsforge_data.get_tree
               ~sp ~root ~with_deleted ?depth () >>= fun t ->
             Ocsforge_xml_tree_dump.xml_of_tree ~sp il_widget t >>= fun t ->
             Lwt.return (t : {{ Ocamlduce.Load.anyxml }})
           end
       | _     -> failwith "Unsuported format") ;
  let rec reg_aux = function
    | [] -> Lwt.return ()
    | (p , n) :: t ->
        (let _ = register_dump_tree_service p n il_widget t_widget in reg_aux t)
  in
  Ocsforge_sql.get_projects_path_list () >>= fun l ->
    reg_aux l




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
          (Ocsforge_lang.t_opt_of_string (fun s -> s))
          (Ocsforge_lang.string_of_t_opt (fun s -> s))
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
          (Ocsforge_lang.t_opt_of_string (fun s -> s))
          (Ocsforge_lang.string_of_t_opt (fun s -> s))
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


let new_project_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~name:"ocsforge_add_project"
    ~options:`Reload
    ~post_params:(
       ((Params.user_type
           Types.task_of_string Types.string_of_task
           "parent") **
        ((Params.string "name") **
          ((Params.user_type
              (Ocsforge_lang.t_opt_of_string Ocsforge_lang.period_of_string)
              (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_period)
              "length") **
           ((Params.user_type
               (Ocsforge_lang.t_opt_of_string Int32.of_string)
               (Ocsforge_lang.string_of_t_opt Int32.to_string)
               "progress") **
            ((Params.user_type
                (Ocsforge_lang.t_opt_of_string Int32.of_string)
                (Ocsforge_lang.string_of_t_opt Int32.to_string)
                "importance") **
             ((Params.user_type
                  (Ocsforge_lang.t_opt_of_string Ocsforge_lang.date_of_string)
                  (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_date)
                  "deadline_t") **
               ((Params.user_type
                   (Ocsforge_lang.t_opt_of_string (fun k -> k))
                   (Ocsforge_lang.string_of_t_opt (fun k -> k))
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
       Data.new_project ~sp ~parent ~name
         ?length ?importance ?deadline ?kind
         ?repository_kind ?repository_path () >>= fun _ -> Lwt.return ()
    )

