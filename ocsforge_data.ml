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
    ~area () >>= fun id ->
      User_sql.add_to_group
        ~user:(Roles.task_creator $ area) ~group:(Roles.task_message_editor_if_author $ id)
      >>= fun () ->
      User_sql.add_to_group
        ~user:(Roles.task_mover_from $ area) ~group:(Roles.task_property_editor $ id)
      >>= fun () ->
      User_sql.add_to_group
        ~user:(Roles.task_mover_to $ area) ~group:(Roles.task_property_editor $ id)
      >>= fun () ->
      User_sql.add_to_group
        ~user:(Roles.kinds_setter $ area) ~group:(Roles.task_property_editor $ id)
      >>= fun () -> Lwt.return id

let blunt_create_area ~forum () =
  Ocsforge_sql.new_area ~forum (*TODO: use kinds !*)

let new_task ~sp ~parent ~subject ~text ~creator
      ?length ?progress ?importance ?deadline_time ?deadline_version ?kind
      ?area () =a (*TODO: get creator with sp*)
  match area with

    | Some None -> (* detach into a new zone *)
        get_area ~task_id:parent () >>= fun parent_area ->
        Roles.get_area_role sp parent_area >>= fun role ->
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
        Roles.get_area_role sp area_inh >>= fun role ->
        !!(role.Roles.task_creator) >>= fun b ->
        if b
        then
          blunt_create_task ~parent ~message ~creator
            ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
            ~area ()
        else Lwt.fail Ocsimore_common.Permission_denied

    | Some Some area -> (* detach into a already existing zone *)
         Roles.get_area_role sp area >>= fun role ->
         !!(role.Roles.task_creator) >>= fun b ->
         if b
         then
           blunt_create_task ~parent ~message ~creator
            ~length ~progress ~importance ~deadline_time ~deadline_version ~kind
            ~area ()
        else Lwt.fail Ocsimore_common.Permission_denied
                   

let get_task ~sp ~task =
  Roles.get_task_role sp task >>= fun role ->
  !!(role.Roles.task_reader) >>= fun b ->
  if b
  then
    Ocsforge_sql.get_task_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied

let get_task_history ~sp ~task =
  Roles.get_task_role sp task >>= fun role ->
  !!(role.Roles.task_reader) >>= fun b ->
  if b
  then
    Ocsforge_sql.get_task_history_by_id ~task_id:task ()
  else Lwt.fail Ocsimore_common.Permission_denied

let get_tasks_by_parent ~sp ~parent =
  () (* needs many role verification... Maybe, task_reader should be a zone property ! maybe zone properties are the only relevant information...*)

