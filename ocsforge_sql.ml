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

let (>>=) = Lwt.bind
module Types = Ocsforge_types



module Defaults =
struct
  let importance = Int32.of_int 20
  let kind = "MISC"
end

(** {2 Database statement and queries.} *)

(*TODO : bootstrap : create a base task for the whole system*)


(** {3 makers } *)

(** {5 make task } *)

let new_task 
      ~parent ~message ~creator ~version
      ?length ?progress
      ?(importance = Defaults.importance)
      ?deadline_time ?deadline_version
      ?(kind       = Defaults.kind      )
      ~area ?area_inheritance
      () =
  let parent_id   = Types.sql_of_task parent in
  let message_id  = Forum_sql.Types.sql_of_message message in
  let creator_id  = User_sql.Types.sql_from_userid creator in
  let area_id     = Types.sql_of_right_area area in
  let now         = CalendarLib.Calendar.now () in
  let area_inh_id = match area_inheritance with
    | None   -> area_id
    | Some a -> Types.sql_of_right_area a
  in
    Sql.full_transaction_block
      (fun db ->
         PGSQL(db)
           "INSERT INTO ocsforge_tasks \
             (parent, message, edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, area_inheritance)
            VALUES ($parent_id, $message_id, $creator_id, $now, $version, \
                    $?length, $?progress, $importance, \
                    $?deadline_time, $?deadline_version, $kind, \
                    $area_id, $area_inh_id)"
       >>= fun () -> Sql.PGOCaml.serial4 db "ocsforge_task_id_seq"
       >>= fun i -> Lwt.return (Types.task_of_sql i))

(** {5 make area} *)

let new_area ~forum () =
  let forum = Forum_sql.Types.sql_of_forum forum in
   Sql.full_transaction_block
    (fun db ->
      PGSQL(db)
       "INSERT INTO ocsforge_right_areas (forum_id) VALUES ($forum)"
     >>= fun () -> serial4 db "ocsforge_right_areas_id_seq"
     >>= fun i  -> Lwt.return (Types.right_area_of_sql i))


(** {3 getters : getting info } *)

(** {5 getters for tasks} *)


let get_task_by_id ?db ~task_id () =
  let task_id = Types.sql_of_task task_id in
  let f db =
         PGSQL(db)
           "SELECT id, parent, message, \
                   edit_author, edit_time, edit_version, \
                   length, progress, importance, \
                   deadline_time, deadline_version, kind, \
                   area, area_inheritance \
            FROM ocsforge_tasks
            WHERE id = $task_id"
       >>= fun r -> Lwt.return (Types.get_task_info r)
  in match db with
    | None    -> Sql.full_transaction_block f
    | Some db -> f db

let get_task_history_by_id ~task_id () =
  let task_id = Types.sql_of_task task_id in
    Sql.full_transaction_block
      (fun db ->
         PGSQL(db)
           "SELECT id, parent, edit_author, edit_time, edit_version, \
                   length, progress, importance, \
                   deadline_time, deadline_version, kind, \
                   area, area_inheritance
            FROM ocsforge_tasks_history
            WHERE id = $task_id"
      >>= fun history -> get_task_info_by_id ~db ~task_id
      >>= fun current ->
        Lwt.return (Types.get_task_info current,
                    Lwt_util.map_serial Types.get_task_history_info history))

let get_tasks_by_parent ?db ~parent () =
  let parent = Types.sql_of_task parent in
  let f db =
    PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, area_inheritance
       FROM ocsforge_tasks
       WHERE parent = $parent"
      >>= fun r -> Lwt.return (Lwt_util.map_serial Types.get_task_info r)
  in match db with
    | None    -> Sql.full_transaction_block f
    | Some db -> f db

let get_tasks_by_editor ?db ~editor () =
  let editor = User_sql.Types.sql_from_userid editor in
  let f db =
    PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, area_inheritance
       FROM ocsforge_tasks
       WHERE edit_author = $editor"
      >>=fun r -> Lwt.return (Lwt_util.map_serial Types.get_task_info r)
  in match db with
    | None    -> Sql.full_transaction_block f
    | Some db -> f db

(*TODO : more getter by attributes (version, time, progress...*)

(** {5 getters for area} *)

let get_area_inheritance ~task_id () =
  Lwt_pool.use Sql.pool
    (fun db ->
       PGSQL(db)
         "SELECT area_inheritance
          FROM ocsforge_tasks
          WHERE id = $parent_id")

let get_area_by_id ~area_id () =
  let area = Types.sql_of_right_area area_id in
    Lwt_pool.use Sql.pool
      (fun db ->
         PGSQL(db)
           "SELECT id, forum_id, version
            FROM ocsforge_right_areas
            WHERE id = $area"
       >>= fun r -> Lwt.return (Types.get_right_area_info r))


(** {3 history management : to record changes } *)

let copy_in_history ~task_id () =
  let task_id = Types.sql_of_task task_id in
  Lwt_pool.use Sql.pool
   (fun db -> 
      PGSQL(db)
      "INSERT INTO ocsforge_tasks_history \
         (id, parent, edit_author, edit_time, edit_version, \
          length, progress, importance, deadline_time, deadline_version, kind, \
          area, area_inheritance)
       SELECT id, parent, edit_author, edit_time, edit_version, \
              length, progress, importance, deadline_time, \
                                                       deadline_version, kind, \
              area, area_inheritance
        FROM ocsforge_tasks
        WHERE id = $task_id"
      >>= fun _ -> Lwt.return ())

let stamp_edition ~task_id ~author =
  let task_id = Types.sql_of_task task_id in
  let editor = User_sql.Types.sql_from_userid author in
  let now = CalendarLib.Calendar.now () in
    Lwt_pool.use Sql.pool
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks \
           SET (edit_author, edit_time, edit_version) = \
               ($editor, $now, $ver) \
           WHERE id = $task_id")
 
(** {3 setters : to tamper recorded tuples } *)

(** {5 setters for tasks} *)

let set_length ~task_id ~length =
  let task_id = Types.sql_of_task task_id in
    Lwt_pool.use Sql.pool
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET length = $length
           WHERE id = $task_id")

let set_progress ~task_id ~progress =
  let task_id = Taypes.sql_of_task task_id in
  let progress = progress in
    Lwt_pool.use Sql.pool
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET progress = $progress
           WHERE id = $task_id")

let set_importance ~task_id ~importance =
  let task_id = Taypes.sql_of_task task_id in
  let importance = importance in
    Lwt_pool.use Sql.pool
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET importance = $importance
           WHERE id = $task_id")

let set_deadline_time ~task_id ~deadline_time =
  let task_id = Taypes.sql_of_task task_id in
    Lwt_pool.use Sql.pool
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET deadline_time = $deadline_time
             WHERE id = $task_id")

let set_deadline_version ~task_id ~deadline_version =
  let task_id = Taypes.sql_of_task task_id in
    Lwt_pool.use Sql.pool
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET deadline_version = $deadline_version
             WHERE id = $task_id")

let set_kind ~task_id ~kind =
  let task_id = Taypes.sql_of_task task_id in
  let kind = Types.string_of_kind kind in
    Lwt_pool.use Sql.pool
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET kind = $kind
             WHERE id = $task_id")

let set_area ~task_id ~area =
  let task_id = Types.sql_of_task task_id in
  let area = Types.sql_of_right_area area in
    Lwt_pool.use Sql.pool
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET area = $area
             WHERE id = $task_id")

let set_area_inheritance ~task_id ~area =
  let task_id = Types.sql_of_task task_id in
  let area = Types.sql_of_right_area area in
    Lwt_pool.use Sql.pool
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET area_inheritance = $area
             WHERE id = $task_id")

(*TODO : multiple field tamprers *)

(** {5 setters for right area} *)

let set_version ~area_id ~version () =
  let area = Types.sql_of_right_area area in
    Lwt_pool.use Sql.pool
      (fun db ->
         PGSQL(db)
           "UPDATE ocsforge_right_areas
            SET version = $version
            WHERE id = $area")

(* let set_kinds ~area_id ~kinds () = () TODO : problem with insertion of multiple values in the same statement*)


(** {3 tree tamperer : change the atributtes of tasks in a whole (sub)tree } *)
(*TODO*)


