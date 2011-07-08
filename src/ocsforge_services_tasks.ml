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

(** @author Raphael Proust *)

let ( ** ) = Eliom_parameters.prod
let ( @@ ) f g = fun x -> f (g x)

module Params = Eliom_parameters
module Data = Ocsforge_data
module Types = Ocsforge_types
module Olang = Ocsforge_lang

open CalendarLib
open Eliom_pervasives


(** Task edition : set values for fields. *)

let set_length_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_length"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string Olang.period_of_string)
          (Olang.string_of_t_opt Olang.string_of_period)
          "length")
      )
    (fun () (task, length) ->
       Data.edit_task ~task ~length () )

let set_progress_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_progress"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string Int32.of_string)
          (Olang.string_of_t_opt Int32.to_string)
          "progress")
      )
    (fun () (task, progress) ->
       Data.edit_task ~task ~progress ())

let set_importance_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_importance"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string Int32.of_string)
          (Olang.string_of_t_opt Int32.to_string)
          "importance")
      )
    (fun () (task, importance) ->
       Data.edit_task ~task ~importance ())

let set_kind_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_kind"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (fun () (task, kind) ->
       Data.edit_task ~task ~kind ())



(** Add a new task with the specified attributes. *)
let new_task_service =
  Eliom_output.Action.register_post_coservice'
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
                   (Olang.t_opt_of_string (fun k -> k))
                   (Olang.string_of_t_opt (fun k -> k))
                   "kind")
               )))))))
    )
    (fun () (parent,
                (subject,
                 (text,
                  (length,
                   (progress,
                    (importance,
                     kind         ))))))
           ->
       let length = Olang.apply_on_opted Calendar.Period.day length in
       Data.new_task ~parent ~subject ~text
          ?length ?progress ?importance ?kind
          () >>= fun _ -> Lwt.return ())


(** Services used by client to get info. *)
let register_dump_tree_service tree_widget task_widget path =
  let _ =
  Eliom_output.Html5.register_service
    ~path:( Neturl.split_path path @ [ "tasks" ; "" ])
    ~get_params:(Params.opt (Params.int32 "id"))
    (fun id () ->
       let p_d = Wiki_widgets_interface.Page_displayable in
       let e404 = Wiki_widgets_interface.Page_404 in
       match id with

         | None ->
             begin
               Data.get_area_for_page ~page:path        >>= fun ri ->
               ((tree_widget#display
                   ~root_task:( Olang.unopt ri.Types.r_root_task )
                ))
                                                            >>= fun content ->
              let gen_box _ = Lwt.return
                (None, content, p_d, Some "Ocsforge - task tree")
              in
              Wiki_site.wikibox_widget#display_container
                ~wiki:ri.Types.r_wiki
                ~menu_style:`Linear
                ~page:((Url.string_of_url_path ~encode:true []), [])
                ~gen_box

              >>= ( Lwt.return @@ fst )
             end

         | Some id ->
             begin
               let task = Types.task_of_sql id in
               ((task_widget#display ~task
                ))   >>= fun c ->
               Data.get_area_for_task ~task          >>= fun ri1 ->
               Data.get_area_for_page ~page:path     >>= fun ri2 ->
               let gen_box _ = Lwt.return
                 (None,
                  c,
                  (if ri1.Types.r_id = ri2.Types.r_id then p_d else e404),
                  Some "Ocsforge - task")
               in
               Wiki_site.wikibox_widget#display_container
                 ~wiki:ri1.Types.r_wiki
                 ~menu_style:`Linear
                 ~page:((Url.string_of_url_path ~encode:true []), [])
                 ~gen_box

               >>= ( Lwt.return @@ fst )
             end
    )
  in Lwt.return ()

type supported_format =
  | Xml

let register_xml_dump_services tree_widget task_widget =
  (* CCC this is not right: it should be a standard XML printer *)
  let _ = Ocsforge_services_source.SourceXmlOutput.register_post_coservice'
    ~name:"ocsforge_task_dump"
    ~post_params:(
      (Params.int32 "root") **
       ((Params.user_type
           ~of_string:(function "xml" -> Xml |_ -> failwith "supported_format")
           ~to_string:(fun Xml -> "xml")
           "format") **
        ((Params.opt (Params.int "depth")) **
         (Params.bool "with_deleted")))
    )
    (fun () (root, (fmt, (depth, with_deleted))) -> match fmt with
       | Xml ->
           let task = Types.task_of_sql root in
           lwt t = Ocsforge_xml_tree_dump.xml_of_tree ~task ~with_deleted ?depth () in
	   Lwt.return [t]
    )
  in
  Ocsforge_sql.get_projects_path_list () >>= fun ppl ->
  Lwt_util.iter_serial
    (register_dump_tree_service tree_widget task_widget)
    ppl



(* Editing fields for right areas.
 * "id" fields are task ids because areas are server side only. *)

let set_repository_path_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_repository_path"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "path")
      )
    (fun () (task, repository_path) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.edit_area ~area ~repository_path ())

let set_repository_kind_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_repository_kind"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.user_type
          (Olang.t_opt_of_string (fun s -> s))
          (Olang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (fun () (task, repository_kind) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.edit_area ~area ~repository_kind ())

let set_version_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_version"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.string "version")
      )
    (fun () (task, version) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.edit_area ~area ~version ())

(** Changing the aviable category of a project. *)
let add_area_kind_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_add_kinds"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.string "kind")
    )
    (fun () (task, kind) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.add_kinds ~area ~kinds:[ kind ])
let del_area_kind_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_del_kinds"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       (Params.string "kind")
    )
    (fun () (task, kind) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.del_kinds ~area ~kinds:[ kind, None ])
let swap_area_kind_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_swap_kinds"
    ~options:`NoReload
    ~post_params:(
       (Params.user_type Types.task_of_string Types.string_of_task "id") **
       ((Params.string "from") **
        (Params.string "to"))
    )
    (fun () (task, (from_, to_)) ->
       Data.get_area_for_task ~task >>= fun { Types.r_id = area } ->
       Data.swap_kinds ~area ~kinds:[ from_, to_ ])



(* The service to create a project. *)
let register_project_service tree_widget task_widget =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_add_project"
    ~options:`NoReload
    ~post_params:(
       ((Params.user_type Types.task_of_string Types.string_of_task "parent") **
        ((Params.string "name") **
          ((Params.user_type
              (Olang.t_opt_of_string int_of_string)
              (Olang.string_of_t_opt string_of_int)
              "length") **
            ((Params.user_type
                (Olang.t_opt_of_string Int32.of_string)
                (Olang.string_of_t_opt Int32.to_string)
                "importance") **
               ((Params.user_type
                   (Olang.t_opt_of_string (fun k -> k))
                   (Olang.string_of_t_opt (fun k -> k))
                   "kind") **
                ((Params.opt (Params.string "repo_kind")) **
                 (Params.opt (Params.string "repo_path"))
               ))))))
    )
    (fun () (parent,
                (name,
                  (length,
                   (importance,
                    (kind,
                     (repository_kind,
                      repository_path))))))
           ->
       let length = Olang.apply_on_opted Calendar.Period.day length in
       Data.new_project ~parent ~name ?length ?importance ?kind
         ?repository_kind ?repository_path
         ~wiki_container:Wiki.default_container_page
         ()                                                     >>= fun task ->
       Data.get_area_for_task ~task                         >>= fun area ->
       Data.new_task
         ~parent:task ~subject:"Unsorted bugs" ~text:""
         ()                                                     >>= fun _ ->
       Ocsforge_sql.get_project_path ~area:area.Types.r_id ()
       >>= (function
              | None -> Lwt.return ()
              | Some path ->
                  (
                   register_dump_tree_service tree_widget task_widget path
                     >>= fun _ ->
                   Ocsforge_services_source.register_repository_service path
                     >>= fun _ ->
                   Lwt.return ()
                  )
           )
    )


(** Separators services *)
(* To get separators, one can use xml tree dumping *)
let insert_separator_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_insert_separator"
    ~options:`NoReload
    ~post_params:(
       Params.user_type Types.task_of_string Types.string_of_task "after" **
       Params.string "content"
     )
    (fun () (after, content) -> Data.insert_separator ~after ~content)
let set_separator_content_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_set_separator_content"
    ~options:`NoReload
    ~post_params:(
       Params.user_type
         Types.separator_of_string
         Types.string_of_separator
         "separator" **
       Params.string "content"
     )
    (fun () (separator, content) ->
       Data.set_separator_content ~separator ~content)
let move_separator_service =
  Eliom_output.Action.register_post_coservice'
    ~name:"ocsforge_move_separator"
    ~options:`NoReload
    ~post_params:(
       Params.user_type
         Types.separator_of_string
         Types.string_of_separator
         "separator" **
       Params.user_type Types.task_of_string Types.string_of_task "after"
     )
    (fun () (separator, after) ->
       Data.move_separator ~separator ~after)

