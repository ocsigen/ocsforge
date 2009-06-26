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

(*Can't compile w/o : *)open Sql


(** {2 Database statement and queries.} *)

(*TODO : bootstrap : create a base task for the whole system*)


(** {3 makers } *)

(** {5 make task } *)

let new_task 
      ~parent ~message ~creator ~version
      ?length        ?progress         ?importance
      ?deadline_time ?deadline_version ?kind
      ~area
      () =
  let deadline_time = Ocsforge_lang.apply_on_opted
                        CalendarLib.Calendar.from_date deadline_time in
  let parent_id     = Types.sql_of_task parent in
  let message_id    = Forum_sql.Types.sql_of_message message in
  let creator_id    = User_sql.Types.sql_from_userid creator in
  let area_id       = Types.sql_of_right_area area in
  let now           = CalendarLib.Calendar.now () in
    Sql.full_transaction_block
      (fun db ->
         PGSQL(db)
           "SELECT tree_min, tree_max
            FROM ocsforge_tasks
            WHERE id = $parent_id"
       >>= (function
         | [] | _::_::_ -> failwith "Ocsforge_sql.new_task not one parent"
         | [(tmin,tmax)] ->
             PGSQL(db)
               "UPDATE ocsforge_tasks \
                SET tree_min = tree_min + 2 \
                WHERE tree_min >= $tmax" >>= fun () ->
             PGSQL(db)
               "UPDATE ocsforge_tasks \
                SET tree_max = tree_max + 2 \
                WHERE tree_max >= $tmax" >>= fun () ->
             PGSQL(db)
               "INSERT INTO ocsforge_tasks \
                 (parent, message, edit_author, edit_time, edit_version, \
                  length, progress, importance, \
                  deadline_time, deadline_version, kind, \
                  area, tree_min, tree_max)
                VALUES ($parent_id, $message_id, $creator_id, $now, $version, \
                        $?length, $?progress, $?importance, \
                        $?deadline_time, $?deadline_version, $?kind, \
                        $area_id, $tmax, ($tmax + 1))")
       >>= fun () -> Sql.PGOCaml.serial4 db "ocsforge_tasks_id_seq"
       >>= fun i -> Lwt.return (Types.task_of_sql i))


(** {5 make area} *)

let new_area ?id ~forum ?(version = "0.0") () =
  let forum = Forum_sql.Types.sql_of_forum forum in
   Sql.full_transaction_block
    (fun db ->
       (match id with
         | None ->
             begin
               PGSQL(db)
                 "SELECT NEXTVAL('ocsforge_right_areas_id_seq')"
               >>= (function
                      | [] | _::_::_ -> failwith "Ocsforge_sql.new_area not one nextval"
                      | [Some id] -> Lwt.return (Int64.to_int32 id)
                      | [None] -> failwith "Ocsforge_sql.new_area nextval returned None")
               >>= fun id ->
                 PGSQL(db)
                   "INSERT INTO ocsforge_right_areas \
                           ( id,  forum_id, version,  inheritance) \
                    VALUES ($id, $forum,    $version, $id)"
              >>= fun () -> Lwt.return id
             end
         | Some id ->
             let id = Types.sql_of_right_area id in
             PGSQL(db)
               "INSERT INTO ocsforge_right_areas \
                       ( id,  forum_id, version,  inheritance) \
                VALUES ($id, $forum,    $version, $id)"
             >>= fun () -> Lwt.return id
       )
     >>= fun i -> Lwt.return (Types.right_area_of_sql i))

let next_right_area_id db =
    PGSQL(db)
      "SELECT NEXTVAL('ocsforge_right_areas_id_seq')"
    >>= (function
           | [] | _::_::_ ->
              failwith "Ocsforge_sql.next_right_area_id not one nextval"
           | [ None ] ->
               failwith "Ocsforge_sql.next_right_area_id nextval returned None"
           | [Some c] ->
               Lwt.return (Types.right_area_of_sql (Int64.to_int32 c)))

let next_task_id db =
  PGSQL (db)
    "SELECT NEXTVAL('ocsforge_tasks_id_seq')"
  >>= (function
         | [] | _::_::_ ->
             failwith "Ocsforge_sql.next_task_id not one nextval"
         | [ None ] ->
             failwith "Ocsforge_sql.next_task_id nextval returned None"
         | [Some c] -> Lwt.return (Types.task_of_sql (Int64.to_int32 c)))
 
(** {3 getters : getting info } *)

(** {5 getters for tasks} *)


let get_task_by_id ~task_id =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
     PGSQL(db)
       "SELECT id, parent, message, \
               edit_author, edit_time, edit_version, \
               length, progress, importance, \
               deadline_time, deadline_version, kind, \
               area, tree_min, tree_max \
        FROM ocsforge_tasks
        WHERE id = $task_id"
     >>= function
         | [r]  -> Lwt.return (Types.get_task_info r)
         | []   -> Lwt.fail Not_found
         | r::_ -> failwith "Ocsforge_sql.get_task_by_id, more than one result")

let get_task_history_by_id ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
       PGSQL(db)
         "SELECT id, parent, edit_author, edit_time, edit_version, \
                 length, progress, importance, \
                 deadline_time, deadline_version, kind, \
                 area
          FROM ocsforge_tasks_history
          WHERE id = $task"
    >>= fun history -> get_task_by_id ~task_id db
    >>= fun current ->
      Lwt.return
        (current,
         List.map Types.get_task_history_info history))

let get_tasks_by_parent ~parent =
  let parent = Types.sql_of_task parent in
    (fun db ->
      PGSQL(db)
        "SELECT id, parent, message, \
                edit_author, edit_time, edit_version, \
                length, progress, importance, \
                deadline_time, deadline_version, kind, \
                area, tree_min, tree_max
         FROM ocsforge_tasks
         WHERE parent = $parent"
        >>= fun r -> (Lwt_util.map_serial
                        (fun t -> Lwt.return (Types.get_task_info t))
                        r))

let get_tasks_in_tree ~root db =
  let root = Types.sql_of_task root in
  PGSQL(db)
    "SELECT tree_min, tree_max
     FROM ocsforge_tasks
     WHERE id = $root"
    >>= (function
           | [] | _::_::_ -> failwith "Ocsforge_sql.get_task_in_tree not unique id"
           | [(tmin, tmax)] ->
  PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, tree_min, tree_max
       FROM ocsforge_tasks
       WHERE tree_min >= $tmin AND tree_max <= $tmax
       ORDER BY tree_min")
    >>= fun r -> (Lwt_util.map_serial
                    (fun t -> Lwt.return (Types.get_task_info t))
                    r)

let get_tasks_by_editor ~editor =
  let editor = User_sql.Types.sql_from_userid editor in
  (fun db ->
    PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, \
              deadline_time, deadline_version, kind, \
              area, tree_min, tree_max
       FROM ocsforge_tasks
       WHERE edit_author = $editor"
      >>=fun r -> (Lwt_util.map_serial
                     (fun t -> Lwt.return (Types.get_task_info t))
                     r))

(*TODO : more getter by attributes (version, time, progress...*)

(** {5 getters for area} *)

let get_area_inheritance ~area_id =
  let id = Types.sql_of_right_area area_id in
    (fun db ->
       PGSQL(db)
         "SELECT inheritance
          FROM ocsforge_right_areas
          WHERE id = $id"
    >>= function
      | [r] -> Lwt.return (Types.right_area_of_sql r)
      | []  -> Lwt.fail Not_found
      | _   -> failwith "Ocsforge_sql.get_area_inheritance, \
                  more than one result")

let get_area_for_task ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
       PGSQL(db)
         "SELECT area
          FROM ocsforge_tasks
          WHERE id = $task"
    >>= function
      | [r] -> Lwt.return (Types.right_area_of_sql r)
      | []  -> Lwt.fail Not_found
      | _   -> failwith "Ocsforge_sql.get_area_inheritance, \
                  more than one result"
    )


let get_area_by_id ~area_id =
  let area = Types.sql_of_right_area area_id in
    (fun db ->
       PGSQL(db)
         "SELECT id, forum_id, version, inheritance
          FROM ocsforge_right_areas
          WHERE id = $area"
     >>= function
       | [r] -> Lwt.return (Types.get_right_area_info r)
       | []  -> Lwt.fail Not_found
       | _   -> failwith "Ocsforge_sql.get_area_by_id, more than one result")
    
let get_area_version ~area_id =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    PGSQL(db)
      "SELECT version
       FROM ocsforge_right_areas
       WHERE id = $area"
    >>= function
      | [r] -> Lwt.return r
      | []  -> Lwt.fail Not_found
      | _   -> failwith "Ocsforge_sql.get_area_version, more than one result"
  )

(** {3 history management : to record changes } *)

let copy_in_history ~task_id db =
  let task_id = Types.sql_of_task task_id in
    PGSQL(db)
      "INSERT INTO ocsforge_tasks_history \
         (id, parent, edit_author, edit_time, edit_version, \
          length, progress, importance, deadline_time, deadline_version, kind, \
          area)
       SELECT id, parent, edit_author, edit_time, edit_version, \
              length, progress, importance, deadline_time, \
                                                       deadline_version, kind, \
              area
        FROM ocsforge_tasks
        WHERE id = $task_id"

let stamp_edition ~task_id ~author db =
  let task = Types.sql_of_task task_id in
  let editor = User_sql.Types.sql_from_userid author in
  let now = CalendarLib.Calendar.now () in
    get_area_for_task ~task_id db >>= fun area_id ->
    get_area_version ~area_id db >>= fun ver ->
      PGSQL(db)
        "UPDATE ocsforge_tasks \
         SET (edit_author, edit_time, edit_version) = \
             ($editor, $now, $ver) \
         WHERE id = $task"
 
(** {3 setters : to tamper recorded tuples } *)

(** {5 setters for tasks} *)

let set_length ~task_id ~length =
  let task_id = Types.sql_of_task task_id in
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET length = $?length
           WHERE id = $task_id")

let set_progress ~task_id ~progress =
  let task_id = Types.sql_of_task task_id in
  let progress = progress in
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET progress = $?progress
           WHERE id = $task_id")

let set_importance ~task_id ~importance =
  let task_id = Types.sql_of_task task_id in
  let importance = importance in
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET importance = $?importance
           WHERE id = $task_id")

let set_deadline_time ~task_id ~deadline_time =
  let deadline_time = Ocsforge_lang.apply_on_opted
                        CalendarLib.Calendar.from_date deadline_time in
  let task_id = Types.sql_of_task task_id in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET deadline_time = $?deadline_time
             WHERE id = $task_id")

let set_deadline_version ~task_id ~deadline_version =
  let task_id = Types.sql_of_task task_id in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET deadline_version = $?deadline_version
             WHERE id = $task_id")

let set_kind ~task_id ~kind =
  let task_id = Types.sql_of_task task_id in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET kind = $?kind
             WHERE id = $task_id")

let set_area ~task_id ~area =
  let task_id = Types.sql_of_task task_id in
  let area = Types.sql_of_right_area area in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET area = $area
             WHERE id = $task_id")

let set_parent ~task_id ~parent =
  let task_id = Types.sql_of_task task_id in
  let parent = Types.sql_of_task parent in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET parent = $parent
             WHERE id = $task_id")

let set_area_inheritance ~area_id ~area_inheritance =
  let area_inh = Types.sql_of_right_area area_inheritance in
  let area_id = Types.sql_of_right_area area_id in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_right_areas
             SET inheritance = $area_inh
             WHERE id = $area_id")

(*TODO : multiple field tamprers *)

(** {5 setters for right area} *)

let set_version ~area_id ~version =
  let area = Types.sql_of_right_area area_id in
      (fun db ->
         PGSQL(db)
           "UPDATE ocsforge_right_areas
            SET version = $version
            WHERE id = $area")


(** {3 tree tamperer : change the atributtes of tasks in a whole (sub)tree } *)
(*TODO*)

let change_tree_marks ~task_id ~parent_id =
  let task     = Types.sql_of_task task_id in
  let parent   = Types.sql_of_task parent_id in
    (fun db ->
       PGSQL(db)
       "SELECT tree_min, tree_max
        FROM ocsforge_tasks
        WHERE id = $task"
     >>= (function
            | [] | _::_::_ -> failwith "Ocsforge_sql.move_task not one task"
            | [(mi,ma)] -> Lwt.return (mi,ma))
     >>= fun (mi,ma) ->
       PGSQL(db)
         "SELECT tree_max
          FROM ocsforge_tasks
          WHERE id = $parent"
     >>= (function
            | [] | _::_::_ -> failwith "Ocsforge_sql.move_task not one parent"
            | [m] -> Lwt.return m)
     >>= fun m ->
       let size = Int32.sub ma mi in
       let dist = Int32.sub m ma  in
         PGSQL(db)
           "UPDATE ocsforge_tasks
            SET tree_min = tree_min + $dist, tree_max = tree_max + $dist
            WHERE tree_min >= $mi AND tree_max <= $ma"
     >>= fun () ->
         PGSQL(db)
           "UPDATE ocsforge_tasks
            SET tree_min = tree_min - $size, tree_max = tree_max - $size
            WHERE tree_min > $ma AND tree_max < $m"
    )


(** {3 Managing Kinds} *)

let get_kinds_for_area ~area_id =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     PGSQL(db)
       "SELECT kind
        FROM ocsforge_task_kinds
        WHERE right_area = $area")


let add_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     let f k =
       PGSQL(db)
         "INSERT INTO ocsforge_task_kinds (right_area, kind) 
          VALUES ($area, $k)"
     in Lwt_util.iter_serial f kinds)



let del_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     let f (k,alt) =
       (match alt with
         | None -> Lwt.return ()
         | Some a ->
             PGSQL(db)
               "UPDATE ocsforge_tasks
                SET kind = $a
                WHERE kind = $k AND area = $area")
         >>= fun () ->
       PGSQL(db)
         "DELETE FROM ocsforge_task_kinds
          WHERE kind = $k AND right_area = $area"
     in Lwt_util.iter_serial f kinds)


let set_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     PGSQL(db)
       "DELETE FROM ocsforge_task_kinds
        WHERE right_area = $area" >>= fun () ->
     add_kinds_for_area ~area_id ~kinds db)

let swap_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     let f (old,nu) =
       PGSQL(db)
         "UPDATE ocsforge_tasks
          SET kind = $nu
          WHERE kind = $old AND area = $area"
     in Lwt_util.iter_serial f kinds)



(**/**)
let bootstrap_task ~area ~message =
  Sql.full_transaction_block
    (fun db ->
       next_task_id db >>= fun id ->
       let id = Types.sql_of_task id in
       let message = Forum_sql.Types.sql_of_message message in
       let area = Types.sql_of_right_area area in
       let version = "0" in
       let kind = "Forge" in
       let now = CalendarLib.Calendar.now () in
       let author = User_sql.Types.sql_from_userid User.admin in
       let tmin = Int32.zero in
       let tmax = Int32.one in
       PGSQL(db)
        "INSERT INTO ocsforge_tasks \
          (id, parent, message, edit_author, edit_time, edit_version, \
           kind, area, tree_min, tree_max)
         VALUES ($id, $id, $message, $author, $now, $version, \
                 $kind, $area, $tmin, $tmax)")

let get_task_count () =
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
         "SELECT COUNT('id') FROM ocsforge_tasks"
          >>= (function
                 | [] | _::_::_ | [None] ->
                     failwith "Ocsforge_sql.get_task_count"
                 | [Some i] -> Lwt.return (Int64.to_int i)))

