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

let (>>=) = Lwt.bind
module Types = Ocsforge_types
module Olang = Ocsforge_lang

(*Can't compile w/o *) open Sql


(** {2 Database statement and queries.} *)

(** {3 makers } *)

(** {5 make task } *)

let new_task 
      ~parent ~message ~creator ~version
      ?length ?progress ?importance ?kind
      ~area ?(area_root = false)
      () =
  let parent_id   = Types.sql_of_task parent in
  let message_id  = Forum_types.sql_of_message message in
  let creator_id  = User_sql.Types.sql_from_userid creator in
  let area_id     = Types.sql_of_right_area area in
  let now         = CalendarLib.Calendar.now () in
    Sql.full_transaction_block
      (fun db ->
         PGSQL(db)
           "SELECT tree_min, tree_max
            FROM ocsforge_tasks
            WHERE id = $parent_id"
       >>= Ocsforge_lang.apply_on_uniq_or_fail
             "Ocsforge_sql.new_task"
            (fun (tmin,tmax) ->
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
                  length, progress, importance, kind, \
                  area, area_root, tree_min, tree_max)
                VALUES ($parent_id, $message_id, $creator_id, $now, $version, \
                        $?length, $?progress, $?importance, $?kind, \
                        $area_id, $area_root, $tmax, ($tmax + 1))")
       >>= fun () -> Sql.PGOCaml.serial4 db "ocsforge_tasks_id_seq"
       >>= fun i -> Lwt.return (Types.task_of_sql i))


(** {5 make area} *)

let new_area ?id ~forum ?(version = "0.0") ?repository_kind ?repository_path
             ?wiki_container ~wiki ~wikibox () =
  let forum = Forum_types.sql_of_forum forum in
  let wiki_container = Olang.apply_on_opted
                         Wiki_types.sql_of_wikibox wiki_container
  in
  let wiki = Wiki_types.sql_of_wiki wiki in
  let wikibox = Wiki_types.sql_of_wikibox wikibox in 
  Sql.full_transaction_block
    (fun db ->
      (match id with
        | None ->
           begin
             PGSQL(db)
               "SELECT NEXTVAL('ocsforge_right_areas_id_seq')"
             >>= (function
                    | [] | _::_::_ ->
                        failwith "Ocsforge_sql.new_area not one nextval"
                    | [Some id] -> Lwt.return (Int64.to_int32 id)
                    | [None] ->
                        failwith "Ocsforge_sql.new_area nextval returned None")
             >>= fun id ->
               PGSQL(db)
                 "INSERT INTO ocsforge_right_areas \
                         ( id,  forum_id, version, \
                          repository_kind, repository_path, \
                          wiki_container, wiki, sources_container) \
                  VALUES ($id, $forum,    $version, \
                          $?repository_kind, $?repository_path, \
                          $?wiki_container, $wiki, $wikibox)"
            >>= fun () -> Lwt.return id
           end
        | Some id ->
           let id = Types.sql_of_right_area id in
           PGSQL(db)
             "INSERT INTO ocsforge_right_areas \
                     ( id,  forum_id, version, \
                      repository_kind, repository_path, \
                      wiki_container, wiki, sources_container) \
              VALUES ($id, $forum,    $version, \
                      $?repository_kind, $?repository_path, \
                      $?wiki_container, $wiki, $wikibox)"
           >>= fun () -> Lwt.return id
      )
    >>= fun i -> Lwt.return (Types.right_area_of_sql i))

let get_path_for_area ~area =
  let area = Types.sql_of_right_area area in
    (fun db ->
       PGSQL(db)
         "SELECT wiki FROM ocsforge_right_areas WHERE id = $area"
       >>= Ocsforge_lang.apply_on_uniq_or_fail
             "Ocsforge_sql.get_path_for_area"
             (fun wikiid ->
                PGSQL(db)
                  "SELECT pages FROM wikis WHERE id = $wikiid")
       >>= (Olang.apply_on_uniq_or_fail "Ocsforge_sql.get_path_for_area"
             (Olang.apply_on_opted_lwt Neturl.split_path)))

let next_right_area_id db =
    PGSQL(db)
      "SELECT NEXTVAL('ocsforge_right_areas_id_seq')"
    >>= Ocsforge_lang.apply_on_uniq_or_fail
          "Ocsforge_sql.newt_right_area_id"
          (fun c -> Lwt.return
             (Types.right_area_of_sql
                (Int64.to_int32
                   (Ocsforge_lang.unopt
                      ~exc:(Failure "Ocsforge_sql.next_right_area_id is Null")
                      c)))
          )

let next_task_id db =
  PGSQL (db)
    "SELECT NEXTVAL('ocsforge_tasks_id_seq')"
    >>= Ocsforge_lang.apply_on_uniq_or_fail
          "Ocsforge_sql.newt_right_area_id"
          (fun c -> Lwt.return
             (Types.task_of_sql
                (Int64.to_int32
                   (Ocsforge_lang.unopt
                      ~exc:(Failure "Ocsforge_sql.next_task_id is Null")
                      c)))
          )
 
(** {3 getters : getting info } *)

(** {5 getters for tasks} *)

let get_task_by_id ~task_id ?(with_deleted = false) =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
     PGSQL(db)
       "SELECT id, parent, message, \
               edit_author, edit_time, edit_version, \
               length, progress, importance, kind, \
               area, tree_min, tree_max, deleted, area_root \
        FROM ocsforge_tasks
        WHERE id = $task_id AND (deleted = $with_deleted OR deleted = false)"
     >>= function
         | [r]     -> Lwt.return (Types.get_task_info r)
         | []      -> Lwt.fail Not_found
         | _::_::_ ->
             failwith "Ocsforge_sql.get_task_by_id, more than one result")

let get_task_history_by_id ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
       PGSQL(db)
         "SELECT id, parent, edit_author, edit_time, edit_version, \
                 length, progress, importance, kind, \
                 area
          FROM ocsforge_tasks_history
          WHERE id = $task"             >>= fun history ->
       get_task_by_id ~task_id db       >>= fun current ->

       Lwt.return (current, List.map Types.get_task_history_info history))

let get_tasks_by_parent ~parent ?(with_deleted = false)=
  let parent = Types.sql_of_task parent in
    (fun db ->
      PGSQL(db)
        "SELECT id, parent, message, \
                edit_author, edit_time, edit_version, \
                length, progress, importance, kind, \
                area, tree_min, tree_max, deleted, area_root
         FROM ocsforge_tasks
         WHERE parent = $parent
           AND (deleted = $with_deleted OR deleted = false)"
        >>= fun r -> Lwt.return (List.map Types.get_task_info r))

let get_tasks_in_tree ~root ?(with_deleted = false) () db =
  let root = Types.sql_of_task root in
  PGSQL(db)
    "SELECT tree_min, tree_max
     FROM ocsforge_tasks
     WHERE id = $root"
    >>= Ocsforge_lang.apply_on_uniq_or_fail
          "Ocsforge_sql.get_task_in_tree"
          (fun (tmin, tmax) ->
             PGSQL(db)
               "SELECT id, parent, message, \
                       edit_author, edit_time, edit_version, \
                       length, progress, importance, kind, \
                       area, tree_min, tree_max, deleted, area_root
                FROM ocsforge_tasks
                WHERE tree_min >= $tmin AND tree_max <= $tmax
                  AND (deleted = $with_deleted OR deleted = false)
                ORDER BY tree_min")
    >>= fun r -> Lwt.return (List.map Types.get_task_info r)

let get_tasks_by_editor ~editor ?(with_deleted = false) () =
  let editor = User_sql.Types.sql_from_userid editor in
  (fun db ->
    PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, kind, \
              area, tree_min, tree_max, deleted, area_root
       FROM ocsforge_tasks
       WHERE edit_author = $editor
         AND (deleted = $with_deleted OR deleted = false)"
      >>=fun r -> Lwt.return (List.map Types.get_task_info r))

(*TODO : more getter by attributes (version, time, progress...) use "comprehensions" *)

(** {5 getters for area} *)


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
      | _   -> failwith "Ocsforge_sql.get_area_for_task more than one result"
    )

let get_area_for_page ~page_id =
  (fun db ->
    PGSQL(db)
      "SELECT id
      FROM ocsforge_right_areas
      WHERE wiki = (SELECT id FROM wikis 
                    WHERE pages=$page_id)
      AND root_task IS NOT NULL"
    >>= function
      | [r] -> Lwt.return (Types.right_area_of_sql r)
      | []  -> Lwt.fail Not_found
      | _   -> failwith "Ocsforge_sql.get_area_for_page more than one result"
    )


let get_area_info_for_task ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
       PGSQL(db)
         "SELECT id, forum_id, version, \
                 repository_kind, repository_path, root_task, \
                 wiki_container, wiki, sources_container
          FROM ocsforge_right_areas
          WHERE id = (SELECT area
                      FROM ocsforge_tasks
                      WHERE id = $task)"
    >>= (Ocsforge_lang.apply_on_uniq_or_fail_lwt
           "Ocsforge_sql.get_area_info_for_task"
           Types.get_right_area_info)
    )

let get_area_info_for_page ~page_id =
  (fun db ->
    PGSQL(db)
      "SELECT id, forum_id, version, \
              repository_kind, repository_path, root_task, \
              wiki_container, wiki, sources_container
      FROM ocsforge_right_areas
      WHERE wiki = (SELECT id
                    FROM wikis
                    WHERE pages = $page_id)
      AND root_task IS NOT NULL"
      >>= (Ocsforge_lang.apply_on_uniq_or_fail_lwt
             "Ocsforge_sql.get_area_info_for_page"
             Types.get_right_area_info)
    )

let get_area_by_id ~area_id =
  let area = Types.sql_of_right_area area_id in
    (fun db ->
       PGSQL(db)
         "SELECT id, forum_id, version, \
                 repository_kind, repository_path, root_task, \
                 wiki_container, wiki, sources_container
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
          length, progress, importance, kind, \
          area)
       SELECT id, parent, edit_author, edit_time, edit_version, \
              length, progress, importance, kind, \
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
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET progress = $?progress
           WHERE id = $task_id")

let set_importance ~task_id ~importance =
  let task_id = Types.sql_of_task task_id in
     (fun db ->
        PGSQL(db)
          "UPDATE ocsforge_tasks
           SET importance = $?importance
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

let set_deleted ~task_id ~deleted =
  let task_id = Types.sql_of_task task_id in
      (fun db -> 
          PGSQL(db)
            "UPDATE ocsforge_tasks
             SET deleted = $deleted
             WHERE id = $task_id")
 

(*TODO : multiple field tamperers *)

(** {5 setters for right area} *)

let set_repository_kind ~area_id ~repository_kind =
  let area_id = Types.sql_of_right_area area_id in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_right_areas
          SET repository_kind = $?repository_kind
          WHERE id = $area_id")
  
let set_repository_path ~area_id ~repository_path =
  let area_id = Types.sql_of_right_area area_id in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_right_areas
          SET repository_path = $?repository_path
          WHERE id = $area_id")

let set_version ~area_id ~version =
  let area = Types.sql_of_right_area area_id in
      (fun db ->
         PGSQL(db)
           "UPDATE ocsforge_right_areas
            SET version = $version
            WHERE id = $area")

let set_root_task ~area_id ~task =
  let area = Types.sql_of_right_area area_id in
  let task = Types.sql_of_task task in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_right_areas
          SET root_task = $task
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
     >>= Ocsforge_lang.apply_on_uniq_or_fail
           "Ocsforge_lang.change_tree_marks"
     (fun (mi,ma) ->
       PGSQL(db)
         "SELECT tree_max
          FROM ocsforge_tasks
          WHERE id = $parent"
     >>= Ocsforge_lang.apply_on_uniq_or_fail
           "Ocsforge_lang.change_tree_marks"
    (fun m ->
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
    )))


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


(** {3 managing separators} *)
let get_separators ~root_task =
  let task = Types.sql_of_task root_task in
  (fun db ->
     PGSQL(db)
       "SELECT tree_min, tree_max
        FROM ocsforge_tasks
        WHERE id = $task"
     >>= Ocsforge_lang.apply_on_uniq_or_fail
          "Ocsforge_sql.get_separators"
          (fun (tmin, tmax) ->
     PGSQL(db)
       "SELECT id, after, content
        FROM ocsforge_tasks_separators
        WHERE after IN (SELECT id
                        FROM ocsforge_tasks
                        WHERE tree_min >= $tmin AND tree_max <= $tmax)"
     >>= fun r -> Lwt.return (List.map Types.get_separator_info r)))
let new_separator ~after ~content =
    (fun db ->
       PGSQL(db)
         "INSERT INTO ocsforge_tasks_separators (after, content)
          VALUES ($after, $content)")
let set_separator_content ~separator ~content =
  let separator = Types.sql_of_separator separator in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_tasks_separators
          SET content = $content
          WHERE id = $separator")
let move_separator ~separator ~after =
  let separator = Types.sql_of_separator separator in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_tasks_separators
          SET after = $after
          WHERE id = $separator")
let get_task_for_separator ~separator =
  let separator = Types.sql_of_separator separator in
  (fun db ->
     PGSQL(db)
      "SELECT id, parent, message, \
              edit_author, edit_time, edit_version, \
              length, progress, importance, kind, \
              area, tree_min, tree_max, deleted, area_root
       FROM ocsforge_tasks
       WHERE tree_max = $separator"
     >>= Olang.apply_on_uniq_or_fail_lwt
           "Ocsforge_sql.get_task_for_separator"
           Types.get_task_info)
let get_area_for_separator ~separator =
  let separator = Types.sql_of_separator separator in
  (fun db ->
     PGSQL(db)
         "SELECT id, forum_id, version, \
                 repository_kind, repository_path, root_task, \
                 wiki_container, wiki, sources_container
          FROM ocsforge_right_areas
          WHERE id = (SELECT area
                      FROM ocsforge_tasks
                      WHERE tree_max = (SELECT after
                                        FROM ocsforge_tasks_separators
                                        WHERE id = $separator))"
     >>= (function
            | [] -> Lwt.fail Not_found
            | _::_::_ ->
               failwith "Ocsforge_sql.get_area_for_separator not uniq result"
            | [ r ] -> Lwt.return (Types.get_right_area_info r)))


let adapt_to_project_spawn ~spawning ~new_area ~old_area =
  let old_area = Types.sql_of_right_area old_area in
  let new_area = Types.sql_of_right_area new_area in
  let spawn    = Types.sql_of_task spawning in
  let t        = true in
    (fun db ->
       PGSQL(db)
         "UPDATE ocsforge_tasks
          SET area_root = $t, area = $new_area
          WHERE id = $spawn"
         >>= fun () ->
       PGSQL(db)
         "UPDATE ocsforge_right_areas
          SET root_task = $spawn
          WHERE id = $old_area"
    )


(**/**)

let find_subject_content ~task =
  let task = Types.sql_of_task task in
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
         "SELECT content
          FROM wikiboxescontent
          WHERE wikibox = (SELECT subject
                           FROM forums_messages
                           WHERE id = (SELECT message
                                       FROM ocsforge_tasks
                                       WHERE id = $task))"
    >>= Ocsforge_lang.apply_on_uniq_or_fail
          "Ocsforge_sql.find_subject_content"
          (fun s -> Lwt.return (Ocsforge_lang.unopt ~default:"" s))
    )


let bootstrap_task ~area ~message =
  Sql.full_transaction_block
    (fun db ->
       next_task_id db >>= fun id_ ->
       let id      = Types.sql_of_task id_ in
       let message = Forum_types.sql_of_message message in
       let area    = Types.sql_of_right_area area in
       let version = "0.0" in
       let kind    = "" in
       let now     = CalendarLib.Calendar.now () in
       let author  = User_sql.Types.sql_from_userid User.admin in
       let tmin    = Int32.zero in
       let tmax    = Int32.one in
       let root    = true in
       PGSQL(db)
        "INSERT INTO ocsforge_tasks \
          (id, parent, message, edit_author, edit_time, edit_version, \
           kind, area, tree_min, tree_max, area_root)
         VALUES ($id, $id, $message, $author, $now, $version, \
                 $kind, $area, $tmin, $tmax, $root)" >>= fun _ ->
       PGSQL(db)
         "UPDATE ocsforge_right_areas \
          SET root_task = $id \
          WHERE id = $area" >>= fun _ ->
       Lwt.return id_)

let get_task_count () =
  (*TODO: when tree management is bugproof, use tree_max & tree_min *)
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
         "SELECT COUNT('id') FROM ocsforge_tasks"
          >>= (function
                 | [] | _::_::_ | [None] ->
                     failwith "Ocsforge_sql.get_task_count"
                 | [Some i] -> Lwt.return (Int64.to_int i)))

let get_root_task () =
  Sql.full_transaction_block
    (fun db ->
       PGSQL(db)
       "SELECT id, parent, message, \
               edit_author, edit_time, edit_version, \
               length, progress, importance, kind, \
               area, tree_min, tree_max, deleted, area_root
        FROM ocsforge_tasks
        WHERE tree_min = 0"
     >>= function
         | [r]  -> Lwt.return (Types.get_task_info r)
         | []   -> Lwt.fail Not_found
         | r::_ -> failwith "Ocsforge_sql.get_root_task, more than one result")


let is_area_root ~task =
  let task = Types.sql_of_task task in
  (fun db ->
     PGSQL(db) "SELECT area_root FROM ocsforge_tasks WHERE id = $task"
       >>= (Ocsforge_lang.apply_on_uniq_or_fail "Ocsforge_sql.is_area_root"
             Lwt.return))

let get_projects_path_list () =
  Sql.full_transaction_block
  (fun db ->
     (PGSQL(db)
        "SELECT pages FROM ocsforge_right_areas, wikis
         WHERE ocsforge_right_areas.wiki = wikis.id AND root_task IS NOT NULL")
     >>= fun l -> Lwt.return ( Olang.filter_map (fun x -> x) l )
  )

let get_project_path ~area () =
  let area = Types.sql_of_right_area area in
  Sql.full_transaction_block
    (fun db ->
       (PGSQL(db)
          "SELECT pages
           FROM ocsforge_right_areas, wikis
           WHERE ocsforge_right_areas.wiki = wikis.id \
             AND root_task IS NOT NULL \
             AND ocsforge_right_areas.id = $area")
    >>= (Ocsforge_lang.apply_on_uniq_or_fail "Ocsforge_sql.get_project_path"
           Lwt.return)
    )


let first_message ~forum ~wiki ~creator ~title_syntax ~text ~content_type =
  let sticky = false in
  let moderated = false in
  let creator_id' = User_sql.Types.sql_from_userid creator in
  let forum_id = Forum_types.sql_of_forum forum in
  Sql.full_transaction_block
    (fun db ->

       (*setting the wikibox for the core of the message*)
       Wiki_sql.new_wikibox
         ~db ~wiki ~author:creator ~comment:"" ~content:""
        ~content_type ()                >>= fun wikibox ->

       (*setting the wikibox for the subject*)
       Wiki_sql.new_wikibox
          ~db ~wiki ~author:creator ~comment:"" ~content:text
          ~content_type:title_syntax () >>= fun subject ->
       Lwt.return (Wiki_types.sql_of_wikibox subject)
                                        >>= fun subject ->

       (*putting data into the database*)
       let wikibox = Wiki_types.sql_of_wikibox wikibox in
       (PGSQL(db) "SELECT NEXTVAL('forums_messages_id_seq')"
          >>= (function
                 | [Some next_id] ->
                     let next_id = Int64.to_int32 next_id in
             PGSQL(db) "INSERT INTO forums_messages \
                    (id,        creator_id,  root_id, forum_id, \
                     subject,  wikibox,  moderated,  sticky) \
             VALUES ($next_id, $creator_id', $next_id, $forum_id, \
                     $subject, $wikibox, $moderated, $sticky)"
               | _ -> Lwt.fail 
                   (Failure
                      "Forum_sql.new_message: error in nextval(id) in table forums_messages"))
       ) >>= fun () -> 
      Sql.PGOCaml.serial4 db "forums_messages_id_seq" >>= fun s ->
      Lwt.return (Forum_types.message_of_sql s)
    )

