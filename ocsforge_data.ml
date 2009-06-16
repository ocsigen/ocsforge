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

module Roles = Ocsforge_roles

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group


(** {2 Database access with verification of permissions} *)

(** {2 Task and Area creation } *)

let blunt_create_task ~parent ~message ~creator
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
      ~area () =
  Ocsforge_sql.new_task ~parent ~message ~creator
    ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
    ~area ()

let blunt_create_area ~forum () =
  Ocsforge_sql.new_area ~forum (*TODO: use kinds !*)

let new_task ~sp ~parent ~subject ~text ~creator
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
      ?area () =a (*TODO: get creator with sp*)
  match area with

    | Some None -> (* detach into a new zone *)
        get_area ~task_id:parent () >>= fun parent_area ->
        Roles.get_role sp parent_area >>= fun role ->
        !!(role.Roles.subarea_creator) >>= fun b ->
        if b then
        begin
          (* create forum *)
          Forum.create_forum
            ~wiki_model:Ocsisite.wikicreole_model (*TODO : give the real wiki model*)
            ~title:"Ocsforge area forum" (*TODO : include area code*)
            ~descr:"Messages about tasks in the area" (*TODO : same*)
            () >>= fun finfo ->
          (* create area *)
          blunt_create_area
            ~forum:(FTypes.sql_of_forum finfo.FTypes.f_id)
            () >>= fun area ->
          (* create message *)
          Forum_data.new_message
           ~sp (*TODO*) ~forum ~creator_id:creator ~subject ~text ()
               >>= fun message ->
          (* create task *)
          blunt_create_task ~parent ~message ~creator
            ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
            ~area ()
        end
        else Lwt.fail Ocsimore_common.Permission_denied

    | None -> (* do not detach *)
        get_area_inheritance ~task_id:parent () >>= fun area_inh ->
        Roles.get_role sp area_inh >>= fun role ->
        !!(role.Roles.task_creator) >>= fun b ->
        if b
        then
          blunt_create_task ~parent ~message ~creator
            ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
            ~area ()
        else Lwt.fail Ocsimore_common.Permission_denied

    | Some Some area -> (* detach into a already existing zone *)
         Roles.get_role sp area >>= fun role ->
         !!(role.Roles.task_creator) >>= fun b ->
         if b
         then
           blunt_create_task ~parent ~message ~creator
            ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
            ~area ()
        else Lwt.fail Ocsimore_common.Permission_denied
                   

let get_task ~sp ~task =
  Ocsforge_sql.get_area ~task () >>= fun area ->
  Roles.get_role sp area >>= fun role ->
  !!(role.Roles.task_reader) >>= fun b ->
  if b
  then
    Ocsforge_sql.get_task_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied

let get_task_history ~sp ~task =
  Ocsforge_sql.get_area ~task () >>= fun area ->
  Roles.get_role sp area >>= fun role ->
  !!(role.Roles.task_reader) >>= fun b ->
  if b
  then
    Ocsforge_sql.get_task_history_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied

let filter_task_list_for_reading sp tl =
  Lwt_util.map_serial
    (fun t -> Lwt.return (Roles.get_role sp t.Types.t_area)) tl
    >>= fun tl_roles ->
  Lwt_util.map_serial
    (fun r -> Lwt.return !!(r.Roles.task_reader)) tl_roles
    >>= fun tl_reader ->
  List.map fst (List.filter (fun (_,r) -> r) (List.combine tl tl_reader))

let get_sub_tasks ~sp ~parent =
  Ocsforge_sql.get_tasks_by_parent ~parent ()
    >>= (filter_task_list_for_reading sp)

let get_tasks_edited_by ~sp ~editor =
  Ocsforge_sql.get_tasks_by_editor ~editor ()
    >>= (filter_task_list_for_reading sp)

let edit_task ~sp ~task
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind =
  if    length           = None
     && progress         = None
     && importance       = None
     && deadline_time    = None
     && deadline_version = None
     && kind             = None
  then Lwt.return ()
  else
    begin
      Ocsforge_sql.get_area ~task () >>= fun area ->
      Roles.get_role sp area >>= fun role ->
      !!(role.Roles.task_property_editor) >>= fun b ->
      if not b
      then
        Lwt.fail Ocsimore_common.Permission_denied
      else
        begin
          let f db =
            let task_id = task in
            User.get_user_data sp >>= fun u ->
            let author = u.User_sql.Types.user_id in
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

let move_task ~task ~parent ?area () =
  let task_id = Types.sql_of_task task in
  let parent_id = Types.sql_of_task parent in
  match area with
    | None -> (* do not detach *)
        begin

        end
    | Some None -> (* detach in a new area *)
    | Some Some a -> (* detach in a known area *)
