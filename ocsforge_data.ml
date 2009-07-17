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

(* TODO :
 * use the real forum wiki_model
 * adapt forum rights (non generic inclusions)*)




module Roles = Ocsforge_roles
module Types = Ocsforge_types
module FTypes = Forum_sql.Types
module FRoles = Forum
(*Can't compile without*)open Sql

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group

let do_sql f = Lwt_pool.use Sql.pool f

(** Local functions *)



(** {2 Database access with verification of permissions} *)

(** {4 Task and Area creation } *)

let new_task ~sp ~parent ~subject ~text
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind () =
  User.get_user_id ~sp >>= fun creator ->

        do_sql (Ocsforge_sql.get_area_for_task ~task_id:parent)
                                                     >>= fun area ->
        Roles.get_area_role sp area                  >>= fun role ->
        !!(role.Roles.task_creator)                  >>= fun b ->
        if b
        then
          Sql.full_transaction_block
          (fun db ->
            Ocsforge_sql.get_area_by_id ~area_id:area db >>= fun ainfo ->
            let version = ainfo.Types.r_version in
            let forum   = ainfo.Types.r_forum   in

            Forum_data.new_message
              ~sp ~forum ~creator_id:creator ~subject ~text ()>>= fun message ->

            Ocsforge_sql.new_task ~parent ~message ~creator ~version
              ?length ?progress ?importance
              ?deadline_time ?deadline_version ?kind ~area ()
          )
        else Lwt.fail Ocsimore_common.Permission_denied

let new_project ~sp ~parent ~name
      ?length ?importance ?deadline ?kind
      ?repository_kind ?repository_path ?wiki_container () =

  do_sql (Ocsforge_sql.get_area_for_task ~task_id:parent) >>= fun parent_area ->
  Roles.get_area_role sp parent_area                      >>= fun role ->
  !!(role.Roles.subarea_creator)                          >>= fun b ->
  if not b
  then
    raise Ocsimore_common.Permission_denied
  else
    Sql.full_transaction_block
      (fun db ->
         begin

           (* get area code *)
           Ocsforge_sql.next_right_area_id db             >>= fun c ->

           let title_syntax = Forum_site.title_syntax in
           (* create forum *)
           Forum.create_forum (*TODO: use Forum_data.new_forum ? *)
              ~wiki_model:Ocsisite.wikicreole_model (*TODO : give the real wiki model*)
              ~title_syntax (*TODO:give the real title_syntax*)
              ~title:("Ocsforge area"^(Types.string_of_right_area c)^" forum")
              ~descr:("Messages about tasks in the area"
                      ^ (Types.string_of_right_area c))
              ()                                          >>= fun finfo ->

          let forum = finfo.FTypes.f_id in
(*          let mwiki = finfo.FTypes.f_messages_wiki in
          let cwiki = finfo.FTypes.f_comments_wiki in
 *)

          (* create wiki *)
          Ocsforge_sql.get_path_for_area ~area:parent_area db >>=
            (function
              | None -> failwith "Ocsforge_data.new_project"
              | Some ppath -> Lwt.return ppath)           >>= fun ppath ->
          User.get_user_id ~sp                            >>= fun author ->

          Wiki.create_wiki
             ~title:("Ocsforge area" ^ (Types.string_of_right_area c) ^ " wiki")
             ~descr:("Wiki for ocsforge area" ^ (Types.string_of_right_area c))
             ~path:( ppath @ [ name ] )
             ~author
             ?container_text:wiki_container
             ~model:Ocsisite.wikicreole_model (*TODO : give the real model *)
             ()
                                                           >>= fun wiki ->

          (* create area *)
            Ocsforge_sql.new_area
              ~id:c ~forum ~wiki ?repository_kind ?repository_path ()
          >>= fun _ -> (* the result can't be anything but [c] *)

(* link rights on area and rights on forum... Needs modifications on forum.ml
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ cwiki))
           [(task_comment_sticky_setter, FRoles.message_sticky_makers);
            (task_comment_reader, FRoles.message_readers);
            (task_comment_moderator, FRoles.message_moderators);
            (task_comment_deletor, FRoles.message_deletors);
            (task_comment_writer, FRoles.message_creator);
            (task_comment_writer_not_moderated, FRoles.message_creators_notmod);
           ] 
         >>= fun () ->
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ mwiki))
           [(task_message_editor_if_author, FRoles.message_editor_if_creator);
            (task_message_editor, FRoles.message_editors);
            (task_creator, FRoles.message_creators);
           ]
         >>= fun () ->

         User.add_to_group ~user:(task_admin $ c) ~group:(forum_admin $ forum)
         >>= fun () ->

*)

          (* create message *)
            Forum_data.new_message
              ~sp ~forum ~creator_id:author ~subject:name ~text:"" ()
          >>= fun message ->

          (* create task *)
            Ocsforge_sql.new_task
              ~parent ~message ~creator:author ~version:"0.0"
              ?length ?importance ?deadline_time:deadline ?kind ~area:c ()
          >>= fun task ->

          (*propagating rights into the new area*)
            Lwt_util.iter_serial
              (fun a -> User.add_to_group
                          ~user:(a $ c) ~group:(a $ parent_area))
                          [Roles.task_reader ;
                           Roles.task_comment_reader ;
                           Roles.task_comment_moderator ;
                           Roles.task_comment_sticky_setter ;
                           Roles.task_comment_deletor ;
                           Roles.task_comment_writer ;
                           Roles.task_comment_writer_not_moderated ;
                           Roles.task_property_editor ;
                           Roles.task_message_editor ;
                           Roles.task_message_editor_if_author ;
                           Roles.task_admin ;
                           Roles.task_creator ;
                           Roles.task_mover ;
                           Roles.task_mover_from ;
                           Roles.task_mover_to ;
                           Roles.subarea_creator ;
                           Roles.kinds_setter ;
                           Roles.version_setter ;
                           Roles.repository_setter ; ]
          >>= fun () -> Lwt.return task             

          end)


let get_task ~sp ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role sp area                             >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
  if b
  then do_sql (Ocsforge_sql.get_task_by_id ~task_id:task)
  else Lwt.fail Ocsimore_common.Permission_denied

let get_area_for_task ~sp:_ ~task =
  do_sql(Ocsforge_sql.get_area_info_for_task ~task_id:task)


let get_task_history ~sp ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role sp area                             >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
  if b
  then do_sql (Ocsforge_sql.get_task_history_by_id ~task_id:task)
  else Lwt.fail Ocsimore_common.Permission_denied

let get_area ~sp:_ ~area =
  do_sql (Ocsforge_sql.get_area_by_id ~area_id:area)

let filter_aux_read sp t =
  Roles.get_area_role sp t.Types.t_area >>= fun role ->
  !!(role.Roles.task_reader)

let filter_aux_edit sp t =
  Roles.get_area_role sp t.Types.t_area >>= fun role ->
  !!(role.Roles.task_property_editor)

let filter_task_list_for_reading sp tl =
 Ocsimore_lib.lwt_filter (filter_aux_read sp) tl

let filter_task_list_for_editing sp tl =
  Ocsimore_lib.lwt_filter (filter_aux_edit sp) tl

let get_tree ~sp ~root ?with_deleted ?depth () = (*TODO: use depth for sql transaction, not only for result sorting*)
  Sql.full_transaction_block
    (Ocsforge_sql.get_tasks_in_tree ~root ?with_deleted ())
    >>= (filter_task_list_for_reading sp)
    >>= fun tl ->
      let rec aux tree = function
        | [] -> Lwt.return tree
        | h::t ->
            aux
              (Types.Tree.insert ~tree ~element:h
                 ~is_parent:(fun p e ->
                               e.Types.t_parent = p.Types.t_id))
              t
      in aux Types.Tree.Nil tl
    >>= fun t ->
      let rec aux tree = function
        | 0 -> tree
        | n -> (match tree with
                  | Types.Tree.Nil -> Types.Tree.Nil
                  | Types.Tree.Node (t, l) ->
                     let l = List.map (fun t -> aux t (pred n))  l in
                     Types.Tree.Node (t, l))
      in
        match depth with
          | None -> Lwt.return t
          | Some depth -> Lwt.return (aux t depth)

let get_sub_tasks ~sp ~parent =
  Sql.full_transaction_block
    (Ocsforge_sql.get_tasks_by_parent ~parent)
  >>= (filter_task_list_for_reading sp)


let get_tasks_edited_by ~sp ~editor =
  Sql.full_transaction_block
    (Ocsforge_sql.get_tasks_by_editor ~editor ())
  >>= (filter_task_list_for_reading sp)


let edit_task ~sp ~task
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind () =
  if    length           = None && progress         = None
     && importance       = None && deadline_time    = None
     && deadline_version = None && kind             = None
  then Lwt.return ()
  else
    begin
      do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)  >>= fun area ->
      Roles.get_area_role sp area                            >>= fun role ->
      !!(role.Roles.task_property_editor)                    >>= fun b ->
      if not b
      then
        Lwt.fail Ocsimore_common.Permission_denied
      else
        begin
          let f db =
            let task_id = task in
            User.get_user_id sp >>= fun author ->
            Ocsforge_sql.copy_in_history ~task_id db >>= fun () ->
            Ocsforge_sql.stamp_edition ~task_id ~author db >>= fun () ->
            (match length with
               | None -> Lwt.return ()
               | Some length ->
                   Ocsforge_sql.set_length ~task_id ~length db) >>= fun () ->
            (match progress with
               | None -> Lwt.return ()
               | Some progress ->
                   Ocsforge_sql.set_progress ~task_id ~progress db) >>= fun () ->
            (match importance with
               | None -> Lwt.return ()
               | Some importance ->
                   Ocsforge_sql.set_importance ~task_id ~importance db) >>= fun () ->
            (match deadline_time with
               | None -> Lwt.return ()
               | Some deadline_time ->
                   Ocsforge_sql.set_deadline_time ~task_id ~deadline_time db) >>= fun () ->
            (match deadline_version with
               | None -> Lwt.return ()
               | Some deadline_version ->
                   Ocsforge_sql.set_deadline_version
                     ~task_id ~deadline_version db) >>= fun () ->
            (match kind with
               | None -> Lwt.return ()
               | Some kind -> Ocsforge_sql.set_kind ~task_id ~kind db)
          in
            Sql.full_transaction_block f
        end
    end

let move_task ~sp ~task ~parent =

  do_sql (Ocsforge_sql.is_area_root ~task) >>= fun area_root ->
  if not area_root

  then
    (* adapt to the new parent inheritance : for a task *)
    Sql.full_transaction_block
      (fun db ->
        (*checking rights*)
        Ocsforge_sql.get_area_for_task ~task_id:parent db >>= fun area_new ->
        Roles.get_area_role ~sp area_new                  >>= fun role_new ->
        !!(role_new.Roles.task_mover_to)                  >>= fun mover_to ->
        Ocsforge_sql.get_area_for_task ~task_id:task db   >>= fun area_old ->
        Roles.get_area_role ~sp area_old                  >>= fun role_old ->
        !!(role_old.Roles.task_mover_from)                >>= fun mover_from ->

           if mover_from && mover_to
           then
           (*executing*)
             User.get_user_id sp >>= fun author ->
             Ocsforge_sql.copy_in_history ~task_id:task db >>= fun () ->
             Ocsforge_sql.stamp_edition ~task_id:task ~author db >>= fun () ->

             Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
             Ocsforge_sql.set_area ~task_id:task ~area:area_new db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)
           else Lwt.fail Ocsimore_common.Permission_denied
         )

  else
    (* keep in the same area : for a project *)
    Sql.full_transaction_block
      (fun db ->
        (*checking rights*)
        Ocsforge_sql.get_area_for_task ~task_id:task db  >>= fun area ->
        Roles.get_area_role ~sp area                     >>= fun role ->
        !!(role.Roles.task_mover)                        >>= fun mover ->

           if mover
           then
           (*executing*)
             User.get_user_id sp >>= fun author ->
             Ocsforge_sql.copy_in_history ~task_id:task db >>= fun () ->
             Ocsforge_sql.stamp_edition ~task_id:task ~author db >>= fun () ->

            (* move message *)
    (*FIXME*)Lwt.return ()
    (*FIXME: Ocsforge_sql.get_task_by_id ~db ~task_id:task () >>= fun tinfo ->
             Forum_data.move_message
              ~sp ~message:(tinfo.Types.t_message) ~forum ~creator_id:author ()*)
            >>= fun _ ->
             Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)    
           else Lwt.fail Ocsimore_common.Permission_denied
         )

let edit_area ~sp ~area ?repository_path ?repository_kind ?version () =
  if repository_path = None && repository_kind = None && version = None
  then Lwt.return ()
  else
    begin
      Roles.get_area_role sp area                            >>= fun role ->
      !!(role.Roles.repository_setter)                       >>= fun b ->
      if not b
      then
        Lwt.fail Ocsimore_common.Permission_denied
      else
        begin
          let f db =
            (match repository_path with
               | None -> Lwt.return ()
               | Some r -> Ocsforge_sql.set_repository_path
                             ~area_id:area ~repository_path:r db) >>= fun () ->
            (match repository_kind with
               | None -> Lwt.return ()
               | Some r -> Ocsforge_sql.set_repository_kind
                             ~area_id:area ~repository_kind:r db) >>= fun () ->
            (match version with
               | None -> Lwt.return ()
               | Some v -> Ocsforge_sql.set_version ~area_id:area ~version:v db)
          in Sql.full_transaction_block f
        end
    end


let make_project ~sp ~task ?repository_kind ?repository_path () =

  (* creating a new area and getting rights right *)
    Sql.full_transaction_block
      (fun db ->

         (* checking rights *)
         Ocsforge_sql.get_area_for_task ~task_id:task db   >>= fun area_old ->
         Roles.get_area_role ~sp area_old                  >>= fun role_old ->
         !!(role_old.Roles.subarea_creator)                >>= fun b ->
         if not b (*we only check subarea_creator as it's the "su" of an area*)
         then Lwt.fail Ocsimore_common.Permission_denied
         else

         begin

           (* get area code *)
           Ocsforge_sql.next_right_area_id db             >>= fun c ->

           let title_syntax = Forum_site.title_syntax in
           (* create forum *)
           Forum.create_forum (*TODO: use Forum_data.new_forum ? *)
              ~wiki_model:Ocsisite.wikicreole_model (*TODO : give the real wiki model*)
              ~title_syntax (*TODO:give the real title_syntax*)
              ~title:("Ocsforge area"^(Types.string_of_right_area c)^" forum")
              ~descr:("Messages about tasks in the area"
                      ^ (Types.string_of_right_area c))
              ()                                          >>= fun finfo ->

          let forum = finfo.FTypes.f_id in
(*          let mwiki = finfo.FTypes.f_messages_wiki in
          let cwiki = finfo.FTypes.f_comments_wiki in
 *)

          (* create wiki *)
            (* getting the path *)
          Ocsforge_sql.get_path_for_area ~area:area_old db    >>=
            (function
              | None -> failwith "Ocsforge_data.new_project"
              | Some ppath -> Lwt.return ppath)               >>= fun ppath ->
            (* getting the rigths *)
          Ocsforge_sql.get_task_by_id ~task_id:task db        >>= fun ti ->
          Ocsforge_sql.get_area_by_id ~area_id:area_old db    >>= fun ai ->
          Forum_sql.get_forum ~forum:ai.Types.r_forum ()      >>= fun fi ->
          Wiki_sql.get_wiki_info_by_id fi.Forum_sql.Types.f_messages_wiki
                                                              >>= fun wi ->
          let rights = Wiki_models.get_rights wi.Wiki_types.wiki_model in
            (* getting the wikibox *)
          Forum_sql.get_message ~message_id:ti.Types.t_message ()
                                                          >>= fun mi ->
            (* finally : getting the content *)
          Wiki_data.wikibox_content
            ~rights ~sp
            (mi.FTypes.m_wikibox)                         >>= fun (_,name,_) ->
          User.get_user_id ~sp                            >>= fun author ->

          let name = Ocsforge_lang.unopt name in
          Wiki.create_wiki
             ~title:("Ocsforge area" ^ (Types.string_of_right_area c) ^ " wiki")
             ~descr:("Wiki for ocsforge area" ^ (Types.string_of_right_area c))
             ~path:( ppath @ [ name ] )
             ~author
             (*TODO: use ~container_text*)
             ~model:Ocsisite.wikicreole_model (*TODO : give the real model *)
             ()
                                                           >>= fun wiki ->

          (* create area *)
            Ocsforge_sql.new_area
              ~id:c ~forum ~wiki ?repository_kind ?repository_path ()
          >>= fun _ -> (* the result can't be anything but [c] *)

(* link rights on area and rights on forum... Needs modifications on forum.ml
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ cwiki))
           [(task_comment_sticky_setter, FRoles.message_sticky_makers);
            (task_comment_reader, FRoles.message_readers);
            (task_comment_moderator, FRoles.message_moderators);
            (task_comment_deletor, FRoles.message_deletors);
            (task_comment_writer, FRoles.message_creator);
            (task_comment_writer_not_moderated, FRoles.message_creators_notmod);
           ] 
         >>= fun () ->
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ mwiki))
           [(task_message_editor_if_author, FRoles.message_editor_if_creator);
            (task_message_editor, FRoles.message_editors);
            (task_creator, FRoles.message_creators);
           ]
         >>= fun () ->

         User.add_to_group ~user:(task_admin $ c) ~group:(forum_admin $ forum)
         >>= fun () ->

*)

          (* create message *)
            Forum_data.new_message
              ~sp ~forum ~creator_id:author ~subject:name ~text:"" ()
          >>= fun message ->

          (*propagating rights into the new area*)
            Lwt_util.iter_serial
              (fun a -> User.add_to_group
                          ~user:(a $ c) ~group:(a $ area_old))
                          [Roles.task_reader ;
                           Roles.task_comment_reader ;
                           Roles.task_comment_moderator ;
                           Roles.task_comment_sticky_setter ;
                           Roles.task_comment_deletor ;
                           Roles.task_comment_writer ;
                           Roles.task_comment_writer_not_moderated ;
                           Roles.task_property_editor ;
                           Roles.task_message_editor ;
                           Roles.task_message_editor_if_author ;
                           Roles.task_admin ;
                           Roles.task_creator ;
                           Roles.task_mover ;
                           Roles.task_mover_from ;
                           Roles.task_mover_to ;
                           Roles.subarea_creator ;
                           Roles.kinds_setter ;
                           Roles.version_setter ;
                           Roles.repository_setter ; ]
          >>= fun () ->

          (*making changes to task and area*)
          Ocsforge_sql.adapt_to_project_spawn
            ~spawning:task
            ~new_area:c ~old_area:area_old
            db
          >>= fun () ->

          (* making changes to subarea's wiki *)
          Ocsforge_sql.get_tasks_in_tree ~root:task ~with_deleted:true () db
          >>= fun subtasks ->
          Lwt_util.iter_serial
            (fun { Types.t_id = t } ->
               Ocsforge_sql.get_area_info_for_task t db >>= fun a ->
               Wiki_sql.get_wiki_info_by_id a.Types.r_wiki >>= fun wi ->
               let path =
                 Ocsforge_lang.insert_after_segment
                   (Neturl.split_path
                      (Ocsforge_lang.unopt wi.Wiki_types.wiki_pages))
                   name
                   ppath
               in
               Wiki_data.update_wiki
                 ~rights
                 ~sp
                 ~path:(Some (Neturl.join_path path))
                 a.Types.r_wiki
            )
            subtasks


          end)



(** Tampering with kinds *)

let get_kinds ~sp:_ ~area =
  Sql.full_transaction_block
  (Ocsforge_sql.get_kinds_for_area ~area_id:area)

let add_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.add_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let del_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.del_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let set_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.set_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let swap_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.swap_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied
