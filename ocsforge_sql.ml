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


(* {2 Type conversion for database IO} *)
module Types =
struct

  (** {3 Semi-abstract type for a task, a right tag and a task history} *)
  type task_arg = [ `Task ]
  type task = task_arg Opaque.int32_t
  type right_area_arg = [ `Right_area ]
  type right_area = right_arg Opaque.int32_t
  type task_history_arg = [ `Task_history ]
  type task_history = task_history_arg Opaque.int32_t


  (** Needed intermediary types. *)
  exception Not_a_percent
  type percent = private int32
  let percent_of_int32 (n : int32) =
    if n <= 100 && n>= 0
    then (n : percent)
    else raise Not_a_percent
  let int32_of_percent (n : percent) = (n : int32) 


  (** {3 For right tags : right management for project tree} *)
  type right_area_info = {
    r_id      : right_area;
    r_forum   : Forum_sql.Types.forum;
    r_version : string;
  }

  let right_area_of_sql (u : int32) = (Opaque.int32_t u : right_area)
  let sql_of_right_area (u : right_area) = Opaque.t_int32 u

  let right_area_of_sql_option (u : int32 option) = 
    (Opaque.int32_t_option u : right_area option)
  let sql_of_right_area_option (u : right_area option) = Opaque.t_int32_option u

  let string_of_right_area i = Int32.to_string (sql_of_right_area i)
  let right_area_of_string s = (Opaque.int32_t (Int32.of_string s) : right_area)

  type raw_right_area_info =
      (int32 * int32)

  let get_right_area_info (id, forum_id, ver) =
    {
      r_id      = right_area_of_sql id;
      r_forum   = Forum_sql.Types.forum_of_sql forum_id;
      r_version = ver
    }


  (** {3 For tasks} *)
  type task_info = {
    t_id        : task;
    t_parent    : task;

    t_message : Forum_sql.Types.message;
    
    t_edit_author  : User_sql.Types.userid;
    t_edit_time    : CalendarLib.Calendar.t;
    t_edit_version : string;

    t_length           : Calendar.Period.t option;
    t_progress         : percent option;
    t_importance       : percent;
    t_deadline_time    : CalendarLib.Calendar.t option;
    t_deadline_version : string option;
    t_kind             : string;

    t_area             : right_area;
    t_area_inheritance : right_area;
  }
    


  let task_of_sql (u : int32) = (Opaque.int32_t u : task)
  let sql_of_task (u : task) = Opaque.t_int32 u

  let task_of_sql_option (u : int32 option) = 
    (Opaque.int32_t_option u : task option)
  let sql_of_task_option (u : task option) = Opaque.t_int32_option u

  let string_of_task i = Int32.to_string (sql_of_task i)
  let task_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

  type raw_task_info =
      (int32 * int32 *
       int32 *
       int32 * CalendarLib.Calendar.t * string *
       CalendarLib.Period.t option * int32 option * int32
         * CalendarLib.Calendar.t option * string option * string *
       int32 * int32)

  let get_task_info
      (id,  parent_id,
       message,
       edit_author, edit_time, edit_version,
       length,  progress,  importance,  deadline_time, deadline_version,  kind,
       area,  area_inheritance)
      = 
    {
      t_id        = task_of_sql id;
      t_parent_id = task_of_sql parent_id;

      t_message = Forum_sql.Types.message_of_sql message;

      t_edit_author  = User_sql.Types.userid_from_sql editor_id;
      t_edit_time    = datetime_edit;
      t_edit_version = edit_version;

      t_length           = length;
      t_progress         = ForgeLang.aplly_on_opted percent_of_int32 progress;
      t_importance       = percent_of_int32 importance;
      t_deadline_time    = deadline_time;
      t_deadline_version = deadline_version;
      t_kind             = task_kind_of_string kind;

      t_area             = right_area_of_sql right_tag;
      t_area_inheritance = right_area_of_sql right_inheritance;
    }


  (** {3 For tasks history} *)
  type task_history_info = {
    th_id        : task;
    th_parent_id : task;

    th_edit_author  : User_sql.Types.userid;
    th_edit_time    : CalendarLib.Calendar.t;
    th_edit_version : string;

    th_length           : Calendar.Period.t option;
    th_progress         : percent option;
    th_importance       : percent;
    th_deadline_time    : CalendarLib.Calendar.t option;
    th_deadline_version : string option;
    th_kind             : string;

    th_area             : right_area;
    th_area_inheritance : right_area;
  }

  let task_history_of_sql (u : int32) = (Opaque.int32_t u : task_history)
  let sql_of_task_history (u : task_history) = Opaque.t_int32 u

  let task_history_of_sql_option (u : int32 option) = 
    (Opaque.int32_t_option u : task_history option)
  let sql_of_task_history_option (u : task_history option) = Opaque.t_int32_option u

  let string_of_task_history i = Int32.to_string (sql_of_task i)
  let task_history_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

  type raw_task_history_info =
      (int32 * int32 *
       int32 * CalendarLib.Calendar.t * string *
       CalendarLib.Period.t option * int32 option * int32
         * CalendarLib.Calendar.t option * string option * string *
       int32 * int32)

  let get_task_history_info
      (id, parent,
       editor, time, edit_version,
       length, progress, importance, deadline_time, deadline_ver, kind, 
       right_zone, right_inheritance) =
    {
    th_id        = task_history_of_sql id ;
    th_parent_id = task_of_sql parent ;

    th_edit_author  = User_sql.Types.userid_from_sql editor ;
    th_edit_time    = datetime ;
    th_edit_version = edit_version ;

    th_length           = length ;
    th_progress         = ForgeLang.apply_on_opted percent_of_int32 progress ;
    th_importance       = percent_of_int32 importance ;
    th_deadline_time    = deadline_time ;
    th_deadline_version = deadline_ver ;
    th_kind             = kind_of_string kind ;

    th_area             = right_area_of_sql right_zone ;
    th_area_inheritance = right_area_of_sql right_inheritance ;
  }

end



module Defaults =
struct
  let importance = Types.percent_of_int32 (Int32.of_int 20)
  let kind = "MISC"
end

(** {2 Database statement and queries.} *)

(*TODO : bootstrap : create a base task for the whole system*)


(** {3 makers } *)

(** {5 make task } *)

let new_task 
      ~parent ~message ~creator
      ?length
      ?(progress   = Defaults.progress  )
      ?(importance = Defaults.importance)
      ?deadline_time ?deadline_version
      ?(kind       = Defaults.kind      )
      ~area
      () =
  let parent_id  = Types.sql_of_task parent in
  let message_id = Forum_sql.Types.sql_of_message message in
  let creator_id = User_sql.Types.sql_from_userid creator in
  let area_id    = Types.sql_of_right_area area in
  let now        = CalendarLib.Calendar.now () in
    Sql.full_transaction_block
      (fun db ->
         PGSQL(db)
           "SELECT (version)
            FROM ocsforge_right_areas
            WHERE id = $area_id"
       >>= fun version ->
         PGSQL(db)
           "INSERT INTO ocsforge_tasks \
             (parent, message, edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, area_inheritance)
            VALUES ($parent, $message, $creator_id, $now, $version, \
                    $length, $progress, $importance, \
                    $deadline_time, $deadline_version, $kind, \
                    $area_id, $area_id)"
       >>= fun () -> serial4 db "ocsforge_task_id_seq"
       >>= fun i -> Lwt.return (Types.task_of_sql i))

(** {5 make area} *)

let new_area ~forum () =
  let forum = Forum_sql.Types.sql_of_forum forum in
   Sql.full_transaction_block
    (fun db ->
      PGSQL(db)
       "INSERT INTO ocsforge_right_areas \
          (forum_id)
        VALUES ($forum)"
     >>= fun () -> serial4 db "ocsforge_right_areas_id_seq"
     >>= fun i  -> Lwt.return (Types.right_area_of_sql i))


(** {3 getters : getting info } *)

(** {5 getters for tasks} *)

let get_task_by_id ?db ~task_id () =
  let task_id = Types.sql_of_task task_id in
  let f db =
         PGSQL(db)
           "SELECT (id, parent, message, \
                    edit_author, edit_time, edit_version, \
                    length, progress, importance, \
                    deadline_time, deadline_version, kind, \
                    area, area_inheritance) \
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
           "SELECT (id, parent, edit_author, edit_time, edit_version, \
                    length, progress, importance, \
                    deadline_time, deadline_version, kind, \
                    area, area_inheritance)
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
      "SELECT (id, parent, message, \
               edit_author, edit_time, edit_version, \
               length, progress, importance, \
               deadline_time, deadline_version, kind, \
               area, area_inheritance)
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
      "SELECT (id, parent, message, \
               edit_author, edit_time, edit_version, \
               length, progress, importance, \
               deadline_time, deadline_version, kind, \
               area, area_inheritance)
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
       (PGSQL(db)
          "SELECT area_inheritance
          FROM ocsforge_tasks
          WHERE id = $parent_id")

let get_area_by_id ~area_id () =
  let area = Types.sql_of_right_area area_id in
    Lwt_pool.use Sql.pool
      (fun db ->
         PGSQL(db)
           "SELECT ()
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
       SELECT (id, parent, edit_author, edit_time, edit_version, \
               length, progress, importance, deadline_time, \
                                                       deadline_version, kind, \
               area, area_inheritance)
       FROM ocsforge_task
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
  let progress = Types.int32_of_percent progress in
    Lwt_pool.use Sql.pool
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET progress = $progress
           WHERE id = $task_id")

let set_importance ~task_id ~importance =
  let task_id = Taypes.sql_of_task task_id in
  let importance = Types.int32_of_percent importance in
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

let set_kinds ~area_id ~kinds () = (*TODO*) (*problem with insertion of multiple values in the same statement*)


(** {3 tree tamperer : change the atributtes of tasks in a whole (sub)tree } *)
(*TODO*)


