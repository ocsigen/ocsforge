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

module U_sql = User_sql
module UTypes = User_sql.Types
module Types = Ocsforge_types


let (>>=) = Lwt.bind
let ($) = UTypes.apply_parameterized_group


(** {2 Ocsforge related groups} *)

(** {3 Tasks related groups} *)


(* XXX find better description *)
let param_right_area_arg = {
  User_sql.Types.param_description = "ocsforge right_area_arg";
  param_display = None;
  find_param_functions = None;
}


let aux_grp name descr =
  Lwt_unix.run
   (U_sql.new_parameterized_group "ocsforge_area" name descr param_right_area_arg)



let task_admin : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_admin" "Have full right on tasks in the area"

(*Edition*)
let task_property_editor
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_property_editor" "Can edit properties of tasks in the area"

let task_message_editor
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_message_editor" "Can edit messages for tasks within the area"

let task_message_editor_if_author
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_message_editor_if_author"
    "Can edit their own messages for tasks within the area"

(*Comments*)
let task_comment_moderator
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_moderator" "Can moderate comments for tasks in the area"

let task_comment_sticky_setter
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_sticky_setter"
    "Can make comments (for tasks in the area) sticky"

let task_comment_deletor
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_deletor" "Can delete comments for tasks in the area"

let task_comment_deletor_if_author
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_deletor_if_author"
    "Can delete their own comments for tasks in the area"

let task_comment_writer
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_writer" "Can write comments for tasks in the area"

let task_comment_writer_not_moderated
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_writer_not_moderated"
    "Can write comments for tasks in the area, ignoring moderation"

(*reading*) 

let task_comment_reader
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_comment_reader" "Can read comments about tasks in the area"

let task_reader
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_reader" "Can see properties of tasks in the area"

(*writing*)

let task_creator
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_creator" "Can create a task in the area"

let task_mover
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_mover" "Can move tasks within the area"

let task_mover_from
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_mover_from" "Can move tasks from the area"

let task_mover_to
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "task_mover_to" "Can move tasks to the area"

let subarea_creator
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "subarea_creator" "Can create sub areas"

let kinds_setter
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "kind_list_setter" "Can modify the list of avaiable kinds"

let version_setter
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "version_setter" "Can modify the version of the area"

let repository_setter
      : Types.right_area_arg UTypes.parameterized_group =
  aux_grp "repository_setter" "Can modify the repository properties of the area"


(** {2 Group Canonical Inclusions } *)

let _ = Lwt_unix.run (

  (*admin*)
  User_sql.add_generic_inclusion
    ~subset:task_mover_to ~superset:task_admin >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:task_mover_from ~superset:task_admin >>= fun () ->
  Lwt_util.iter_serial
    (fun superset ->
       User_sql.add_generic_inclusion ~subset:task_admin ~superset)
    [task_property_editor ;
     task_comment_moderator ;
     task_comment_sticky_setter ;
     task_comment_deletor ;
     task_comment_writer_not_moderated ;
     task_message_editor ; ] >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:task_comment_writer_not_moderated ~superset:task_comment_writer
    >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:task_comment_deletor ~superset:task_comment_deletor_if_author
    >>= fun () ->

  User_sql.add_generic_inclusion
    ~subset:task_message_editor ~superset:task_message_editor_if_author
    >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:task_message_editor_if_author ~superset:task_creator
    >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:task_property_editor ~superset:task_creator
    >>= fun () ->


  Lwt_util.iter_serial
    (fun subset ->
       User_sql.add_generic_inclusion ~subset ~superset:task_comment_reader)
    [task_comment_moderator ;
     task_comment_sticky_setter ;
     task_comment_deletor_if_author ;
     task_comment_writer] >>= fun () ->

  (*'everyone' is task reader*)
  Lwt_util.iter_serial
    (fun subset ->
       User_sql.add_generic_inclusion ~subset ~superset:task_reader)
    [task_comment_reader ;
     task_creator ;
     version_setter ;] >>= fun () ->


  User_sql.add_generic_inclusion
    ~subset:task_mover ~superset:task_mover_from >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:task_mover ~superset:task_mover_to   >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:subarea_creator ~superset:task_mover >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:subarea_creator ~superset:repository_setter >>= fun () ->
  User_sql.add_generic_inclusion
    ~subset:kinds_setter ~superset:task_property_editor

)




(** {2 Session data} *)

type role =
    {
      task_reader : bool Lwt.t Lazy.t ;

      task_comment_reader : bool Lwt.t Lazy.t ;
      task_comment_moderator : bool Lwt.t Lazy.t ;
      task_comment_sticky_setter : bool Lwt.t Lazy.t ;
      task_comment_deletor : bool Lwt.t Lazy.t ;
      task_comment_writer : bool Lwt.t Lazy.t ;
      task_comment_writer_not_moderated : bool Lwt.t Lazy.t ;

      task_property_editor : bool Lwt.t Lazy.t ;
      task_message_editor : bool Lwt.t Lazy.t ;
      task_message_editor_if_author : bool Lwt.t Lazy.t ;

      task_admin : bool Lwt.t Lazy.t ;

      task_creator : bool Lwt.t Lazy.t ;

      task_mover : bool Lwt.t Lazy.t ;
      task_mover_from : bool Lwt.t Lazy.t ;
      task_mover_to : bool Lwt.t Lazy.t ;

      subarea_creator : bool Lwt.t Lazy.t ;

      kinds_setter : bool Lwt.t Lazy.t ;
      version_setter : bool Lwt.t Lazy.t ;
      repository_setter : bool Lwt.t Lazy.t ;
    }

let get_role ~area =
  User.get_user_id () >>= fun userid ->
  let user = UTypes.basic_user userid in
  let in_grp grp id = User.in_group ~user ~group:(grp $ id) () in
  let no = lazy (Lwt.return false) in
  let yes = lazy (Lwt.return true) in

  User.in_group () ~group:(task_reader $ area) >>= fun read ->
    (*plan is :
       if task_reader
       then if comment_reader
            then { ... many  tests ... }
            else { ... a few tests ... }
       else { ... no ... }
       
       possible optimisation : test for admin ?
     *)
  if read
  then
    begin
      User.in_group
        ~group:(task_comment_reader $ area) ()
      >>= fun readc ->
       if readc 
       then Lwt.return
       {
         task_reader         = yes ;
         task_comment_reader = yes ;
         task_comment_moderator =
           lazy (in_grp task_comment_moderator area) ;
         task_comment_sticky_setter =
           lazy (in_grp task_comment_sticky_setter area) ;
         task_comment_deletor =
           lazy (in_grp task_comment_deletor area) ;
         task_comment_writer =
           lazy (in_grp task_comment_writer area) ;
         task_comment_writer_not_moderated =
           lazy (in_grp task_comment_writer_not_moderated area) ;
         task_property_editor =
           lazy (in_grp task_property_editor area) ;
         task_message_editor =
           lazy (in_grp task_message_editor area) ;
         task_message_editor_if_author =
           lazy (in_grp task_message_editor_if_author area);
         task_admin        = lazy (in_grp task_admin      area ) ;
         task_creator      = lazy (in_grp task_creator    area ) ;
         task_mover        = lazy (in_grp task_mover      area ) ;
         task_mover_from   = lazy (in_grp task_mover_from area ) ;
         task_mover_to     = lazy (in_grp task_mover_to   area ) ;
         subarea_creator   = lazy (in_grp subarea_creator area ) ;
         kinds_setter      = lazy (in_grp kinds_setter    area ) ;
         version_setter    = lazy (in_grp version_setter  area ) ;
         repository_setter = lazy (in_grp repository_setter  area ) ;
       }
       else Lwt.return
       {
         task_reader = yes ;

         task_comment_moderator            = no ;
         task_comment_reader               = no ;
         task_comment_sticky_setter        = no ;
         task_comment_deletor              = no ;
         task_comment_writer               = no ;
         task_comment_writer_not_moderated = no ;

         task_property_editor =
           lazy (in_grp task_property_editor area) ;
         task_message_editor =
           lazy (in_grp task_message_editor area) ;
         task_message_editor_if_author =
           lazy (in_grp task_message_editor_if_author area);

         task_admin        = no ;
         task_creator      = lazy (in_grp task_creator    area ) ;
         task_mover        = no ;
         task_mover_from   = no ;
         task_mover_to     = no ;
         subarea_creator   = no ;
         kinds_setter      = lazy (in_grp kinds_setter    area ) ;
         version_setter    = lazy (in_grp version_setter  area ) ;
         repository_setter = lazy (in_grp repository_setter  area ) ;
       }
    end
  else Lwt.return
         {
           task_reader                       = no ;
           task_comment_reader               = no ;
           task_comment_sticky_setter        = no ;
           task_comment_moderator            = no ;
           task_comment_deletor              = no ;
           task_comment_writer               = no ;
           task_comment_writer_not_moderated = no ;
           task_property_editor              = no ;
           task_message_editor               = no ;
           task_message_editor_if_author     = no ;
           task_admin         = no ;
           task_creator       = no ;
           task_mover         = no ;
           task_mover_from    = no ;
           task_mover_to      = no ;
           subarea_creator    = no ;
           kinds_setter       = no ;
           version_setter     = no ;
           repository_setter  = no ;
         }



module RoleMap = Map.Make (struct
                             type t = Types.right_area
                             let compare = compare
                           end)

let blunt_sd () =
  let cache = ref RoleMap.empty in
    fun k ->
      Lwt.catch
        (fun () -> Lwt.return (RoleMap.find k !cache))
        (function
           | Not_found ->
               (get_role ~area:k >>= fun role ->
                cache := RoleMap.add k role !cache ;
                Lwt.return role)
           | exc -> (failwith ("Ocsforge.blunt_sd : "
                               ^(Printexc.to_string exc)))
        )


type area_sd = Types.right_area -> role Lwt.t

(** The polytable key for retrieving data inside session data *)
let area_key : area_sd Polytables.key = Polytables.make_key ()

let get_area_session_data () =
  let rc = Eliom_request_info.get_request_cache () in
  try
    Polytables.get ~table:rc ~key:area_key
  with Not_found ->
    let asd = blunt_sd () in
    Polytables.set rc area_key asd;
    asd


(** This is the only function to be called outside of this module ! *)
let get_area_role k =
  let area_sd = get_area_session_data () in
  area_sd k



