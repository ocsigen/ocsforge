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
(*Can't compile without*)open Sql
module FTypes = Forum_sql.Types
module FRoles = Forum

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group


(** Local functions *)

(**[infer_progress ~task] evaluates as [(m,n)] where :
  * [n] is the number of subtasks with progress unset (being [None])
  * [m] is the mean of subtasks' progress (when not being [None])*)
let infer_progress ~sp ~task =
  Ocsforge_sql.get_area_for_task~task_id:task () >>= fun area ->
  Roles.get_area_role sp area >>= fun role ->
  !!(role.Roles.subarea_creator) >>= fun b ->
  if b
  then
    begin
      Ocsforge_sql.get_tasks_by_parent ~parent:task ()
      >>= (function
        | [] -> failwith "Ocsforge_data.infer_progress can't infer on leaf"
        | tasks  ->
        begin
          Lwt_util.map_serial
            (fun {Types.t_progress = progress ;
                  Types.t_tree_min = tmin ;
                  Types.t_tree_max = tmax }
              -> Lwt.return (progress, (Int32.sub tmax tmin)))
            tasks                                     >>= fun tasks ->
          Lwt_util.fold_left
            (fun (total, curr, nones) (value,weight)
              -> match value with
                | None ->
                     Lwt.return (total, curr, Int32.succ nones)
                | Some value ->
                    (Lwt.return (Int32.add total weight) >>= fun total ->
                     Lwt.return
                       (Int32.add curr (Int32.mul value weight)) >>= fun curr ->
                     Lwt.return (total, curr, nones)))
            (Int32.zero, Int32.zero, Int32.zero) tasks
        end)
        >>= fun (t,c,n) -> 
          Lwt.catch
            (fun () -> Lwt.return ((Int32.div c t),n))
            (function
               | Division_by_zero -> Lwt.return (Int32.zero,n)
               | exc -> Lwt.fail exc)
    end
  else Lwt.fail Ocsimore_common.Permission_denied



(** {2 Database access with verification of permissions} *)

(** {4 Task and Area creation } *)

let new_task ~sp ~parent ~subject ~text
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
      ?area () =
  User.get_user_id ~sp >>= fun creator ->
    match area with

    | Some None -> (* detach into a new zone *)
        Ocsforge_sql.get_area_for_task ~task_id:parent () >>= fun parent_area ->
        Roles.get_area_role sp parent_area >>= fun role ->
        !!(role.Roles.subarea_creator) >>= fun b ->
        if b
        then
          Sql.full_transaction_block
          (fun db ->
           begin

          (*get area code*)
             Ocsforge_sql.next_right_area_id ~db
          >>= fun c ->

          (* create forum *)
            Forum.create_forum (*TODO: use Forum_data.new_forum ? *)
              ~wiki_model:Ocsisite.wikicreole_model (*TODO : give the real wiki model*)
              ~title:("Ocsforge area"^(Types.string_of_right_area c)^" forum")
              ~descr:("Messages about tasks in the area"
                      ^ (Types.string_of_right_area c))
              ()

          >>= fun finfo ->
          let forum = finfo.FTypes.f_id in
(*          let mwiki = finfo.FTypes.f_messages_wiki in
          let cwiki = finfo.FTypes.f_comments_wiki in
 *)
          (* create area *)
            Ocsforge_sql.new_area
              ~id:c ~forum ()
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
              ~sp ~forum ~creator_id:creator ~subject ~text ()
          >>= fun message ->

          (* create task *)
            Ocsforge_sql.new_task ~parent ~message ~creator ~version:"0.0"
              ?length ?progress ?importance
              ?deadline_time ?deadline_version ?kind
              ~area:c ()
          >>= fun task ->

          (*propagating rights into the new area*)
            Ocsforge_sql.get_area_inheritance ~area_id:c db
          >>= fun inh ->
            Lwt_util.iter_serial
              (fun a -> User.add_to_group ~user:(a $ c) ~group:(a $ inh))
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
               Roles.version_setter ; ]
          >>= fun () -> Lwt.return task             

          end)

       else Lwt.fail Ocsimore_common.Permission_denied

    | None -> (* do not detach *)
        Ocsforge_sql.get_area_for_task ~task_id:parent ()
                                                     >>= fun area_parent ->
        Sql.full_transaction_block
          (fun db -> Ocsforge_sql.get_area_inheritance ~area_id:area_parent db)
                                                     >>= fun area ->
        Roles.get_area_role sp area                  >>= fun role ->
        !!(role.Roles.task_creator)                  >>= fun b ->
        if b
        then
          Sql.full_transaction_block
          (fun _ ->
            Ocsforge_sql.get_area_by_id ~area_id:area ()  >>= fun ainfo ->
            let version = ainfo.Types.r_version in
            let forum   = ainfo.Types.r_forum   in

            Forum_data.new_message
              ~sp ~forum ~creator_id:creator ~subject ~text ()>>= fun message ->

            Ocsforge_sql.new_task ~parent ~message ~creator ~version
              ?length ?progress ?importance
              ?deadline_time ?deadline_version ?kind ~area ()
          )
        else Lwt.fail Ocsimore_common.Permission_denied

    | Some Some area -> (* override the default inheritance *)
         Roles.get_area_role sp area  >>= fun role ->
         !!(role.Roles.task_creator)  >>= fun b ->
         if b
         then
          Sql.full_transaction_block
          (fun _ ->
            Ocsforge_sql.get_area_by_id ~area_id:area () >>= fun ainfo ->
            let version = ainfo.Types.r_version in
            let forum = ainfo.Types.r_forum in

            Forum_data.new_message
              ~sp ~forum ~creator_id:creator ~subject ~text ()>>= fun message ->

            Ocsforge_sql.new_task ~parent ~message ~creator ~version
              ?length ?progress ?importance
              ?deadline_time ?deadline_version ?kind ~area ()
          )
        else Lwt.fail Ocsimore_common.Permission_denied




let get_task ~sp ~task =
  Ocsforge_sql.get_area_for_task ~task_id:task ()  >>= fun area ->
  Roles.get_area_role sp area                      >>= fun role ->
  !!(role.Roles.task_reader)                       >>= fun b ->
  if b
  then Ocsforge_sql.get_task_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied


let get_task_history ~sp ~task =
  Ocsforge_sql.get_area_for_task ~task_id:task ()  >>= fun area ->
  Roles.get_area_role sp area                      >>= fun role ->
  !!(role.Roles.task_reader)                       >>= fun b ->
  if b
  then Ocsforge_sql.get_task_history_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied

let get_area ~sp:_ ~area =
  Ocsforge_sql.get_area_by_id ~area_id:area ()

let get_inheritance ~sp:_ ~area =
  Sql.full_transaction_block
  (Ocsforge_sql.get_area_inheritance ~area_id:area)

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

let get_tree ~sp ~root =
  Sql.full_transaction_block (Ocsforge_sql.get_tasks_in_tree ~root)
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


let get_sub_tasks ~sp ~parent =
  Ocsforge_sql.get_tasks_by_parent ~parent ()
    >>= (filter_task_list_for_reading sp)


let get_tasks_edited_by ~sp ~editor =
  Ocsforge_sql.get_tasks_by_editor ~editor ()
    >>= (filter_task_list_for_reading sp)


let edit_task ~sp ~task
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind () =
  if    length           = None && progress         = None
     && importance       = None && deadline_time    = None
     && deadline_version = None && kind             = None
  then Lwt.return ()
  else
    begin
      Ocsforge_sql.get_area_for_task ~task_id:task () >>= fun area ->
      Roles.get_area_role sp area                     >>= fun role ->
      !!(role.Roles.task_property_editor)             >>= fun b ->
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

let move_task ~sp ~task ~parent ?area () =
  match area with
    | None        -> (* adapt to the new parent inheritance *)
        Sql.full_transaction_block
         (fun db ->
         (*checking rights*)
           Ocsforge_sql.get_area_for_task ~db ~task_id:parent ()
                                                          >>= fun parent_area ->
           Ocsforge_sql.get_area_inheritance ~area_id:parent_area db
                                                          >>= fun area ->
           Roles.get_area_role ~sp area                   >>= fun role_nu ->
           !!(role_nu.Roles.task_mover_to)                >>= fun mover_to ->
           Ocsforge_sql.get_area_for_task ~db ~task_id:task ()
                                                          >>= fun area_old ->
           Roles.get_area_role ~sp area_old               >>= fun role_old ->
           !!(role_old.Roles.task_mover_from)             >>= fun mover_from ->

           if mover_from && mover_to
           then
           (*executing*)
             User.get_user_id sp >>= fun author ->
             Ocsforge_sql.copy_in_history ~task_id:task db >>= fun () ->
             Ocsforge_sql.stamp_edition ~task_id:task ~author db >>= fun () ->

             Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
             Ocsforge_sql.set_area ~task_id:task ~area db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)
           else Lwt.fail Ocsimore_common.Permission_denied
         )

    | Some area -> (* place in a known area *)
        Sql.full_transaction_block
         (fun db ->
           (*checking rights*)
           Ocsforge_sql.get_area_for_task ~db ~task_id:task ()
                                                      >>= fun area_old ->
           Roles.get_area_role ~sp area_old           >>= fun role_old ->
           !!(role_old.Roles.task_mover_from)         >>= fun mover_from ->
           Roles.get_area_role ~sp area               >>= fun role_nu ->
           !!(role_nu.Roles.task_mover_to)            >>= fun mover_to ->

           if mover_to && mover_from
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
             Ocsforge_sql.set_area ~task_id:task ~area db >>= fun () ->
             Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)    
           else Lwt.fail Ocsimore_common.Permission_denied
         )

let detach_task ~sp ~task ?parent () =
  Sql.full_transaction_block
    (fun db ->
       (*checking rights*)
       (match parent with
          | None ->
              (Ocsforge_sql.get_task_by_id
                 ~db:db ~task_id:task ()               >>= fun tinfo ->
               Ocsforge_sql.get_area_for_task
                 ~db ~task_id:tinfo.Types.t_parent ()  >>= fun superarea ->
               Roles.get_area_role ~sp superarea)
          | Some p ->
              (Ocsforge_sql.get_area_for_task
                 ~db ~task_id:p ()             >>= fun superarea ->
               Roles.get_area_role ~sp superarea))
       >>= fun role ->
       !!(role.Roles.subarea_creator) >>= fun b ->
       if not b

       (*permission denied*)
       then Lwt.fail Ocsimore_common.Permission_denied

       (*exeuting*)
       else
        (
         User.get_user_id sp >>= fun author ->
         Ocsforge_sql.copy_in_history ~task_id:task db >>= fun () ->
         Ocsforge_sql.stamp_edition ~task_id:task ~author db >>= fun () ->

         Ocsforge_sql.next_right_area_id ~db
        >>= fun c ->

        (* create forum *)
         Forum.create_forum
           ~wiki_model:Ocsisite.wikicreole_model (*TODO : give the real wiki model*)
           ~title:("Ocsforge area"^(Types.string_of_right_area c)^" forum")
           ~descr:("Messages about tasks in the area"
                   ^ (Types.string_of_right_area c))
           ()
        >>= fun finfo -> let forum = finfo.FTypes.f_id in

        (* create area *)
          Ocsforge_sql.new_area
            ~id:c ~forum ()
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
        (* move message *)
 (*FIXME*)Lwt.return ()
 (*FIXME: Ocsforge_sql.get_task_by_id ~db ~task_id:task () >>= fun tinfo ->
          Forum_data.move_message
           ~sp ~message:(tinfo.Types.t_message) ~forum ~creator_id:author ()*)
        >>= fun _ ->

        (* move task *)
          (match parent with
             | None -> Lwt.return ()
             | Some parent ->
                begin
                  Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
                  Ocsforge_sql.change_tree_marks
                    ~task_id:task ~parent_id:parent db
                end
          )
        >>= fun () ->
          Ocsforge_sql.set_area ~task_id:task ~area:c db
        >>= fun () ->

          (*propagating rights*)
            Ocsforge_sql.get_area_inheritance ~area_id:c db
          >>= fun inh ->
            Lwt_util.iter_serial
              (fun a -> User_sql.add_to_group ~user:(a $ c) ~group:(a $ inh))
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
              ]
    ))


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
      (fun db -> Ocsforge_sql.add_kinds_for_area ~area_id:area ~kinds db)
  else Lwt.fail Ocsimore_common.Permission_denied

let del_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (fun db -> Ocsforge_sql.del_kinds_for_area ~area_id:area ~kinds db)
  else Lwt.fail Ocsimore_common.Permission_denied

let set_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (fun db -> Ocsforge_sql.set_kinds_for_area ~area_id:area ~kinds db)
  else Lwt.fail Ocsimore_common.Permission_denied

let swap_kinds ~sp ~area ~kinds =
  Roles.get_area_role ~sp area >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (fun db -> Ocsforge_sql.swap_kinds_for_area ~area_id:area ~kinds db)
  else Lwt.fail Ocsimore_common.Permission_denied


