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


(**Insert a new task in the database.
  * Default values are :
  * * importance = 20
  * * kind = "MISC"
  *)
val new_task :
  parent:Ocsforge_types.task ->
  message:Forum_sql.Types.message ->
  creator:User_sql.Types.userid ->
  version:string ->
  ?length:CalendarLib.Calendar.Period.t ->
  ?progress:int32 ->
  ?importance:int32 ->
  ?deadline_time:CalendarLib.Calendar.t ->
  ?deadline_version:string ->
  ?kind:string ->
  area:Ocsforge_types.right_area ->
  unit -> Ocsforge_types.task Lwt.t

(**Insert a new area in the database*)
val new_area :
  ?id:Ocsforge_types.right_area ->
  forum:Forum_sql.Types.forum ->
  ?version:string ->
  unit -> Ocsforge_types.right_area Lwt.t

val next_right_area_id :
  db:Sql.db_t -> Ocsforge_types.right_area Lwt.t

val get_task_by_id :
  ?db:Sql.db_t ->
  task_id:Ocsforge_types.task ->
  unit -> Ocsforge_types.task_info Lwt.t

val get_task_history_by_id :
  task_id:Ocsforge_types.task ->
  unit -> (Ocsforge_types.task_info * Ocsforge_types.task_history_info list) Lwt.t

val get_tasks_by_parent :
  ?db:Sql.db_t ->
  parent:Ocsforge_types.task ->
  unit -> Ocsforge_types.task_info list Lwt.t

val get_tasks_by_editor :
  ?db:Sql.db_t ->
  editor:User_sql.Types.userid ->
  unit -> Ocsforge_types.task_info list Lwt.t

val get_area_inheritance :
  area_id:Ocsforge_types.right_area ->
  Sql.db_t -> Ocsforge_types.right_area Lwt.t

val get_area_for_task :
  ?db:Sql.db_t ->
  task_id:Ocsforge_types.task ->
  unit -> Ocsforge_types.right_area Lwt.t

val get_area_by_id :
  area_id:Ocsforge_types.right_area ->
  unit -> Ocsforge_types.right_area_info Lwt.t

val get_area_version :
  ?db:Sql.db_t ->
  area_id:Ocsforge_types.right_area ->
  unit -> string Lwt.t

val copy_in_history :
  task_id:Ocsforge_types.task ->
  Sql.db_t -> unit Lwt.t

val stamp_edition :
  task_id:Ocsforge_types.task ->
  author:User_sql.Types.userid ->
  Sql.db_t -> unit Lwt.t


val set_length :
  task_id:Ocsforge_types.task ->
  length:CalendarLib.Calendar.Period.t ->
  Sql.db_t -> unit Lwt.t

val set_progress :
  task_id:Ocsforge_types.task ->
  progress:int32 ->
  Sql.db_t -> unit Lwt.t

val set_importance :
  task_id:Ocsforge_types.task ->
  importance:int32 ->
  Sql.db_t -> unit Lwt.t

val set_deadline_time :
  task_id:Ocsforge_types.task ->
  deadline_time:CalendarLib.Calendar.t ->
  Sql.db_t -> unit Lwt.t

val set_deadline_version :
  task_id:Ocsforge_types.task ->
  deadline_version:string ->
  Sql.db_t -> unit Lwt.t

val set_kind :
  task_id:Ocsforge_types.task ->
  kind:string ->
  Sql.db_t -> unit Lwt.t

val set_area :
  task_id:Ocsforge_types.task ->
  area:Ocsforge_types.right_area ->
  Sql.db_t -> unit Lwt.t

val set_parent :
  task_id:Ocsforge_types.task ->
  parent:Ocsforge_types.task ->
  Sql.db_t -> unit Lwt.t

(** When moving a task to a new parent, updates tree_min and tree_max fields. *)
val change_tree_marks :
  task_id:Ocsforge_types.task ->
  parent_id:Ocsforge_types.task -> Sql.db_t -> unit Lwt.t


val add_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:string list ->
  Sql.db_t -> unit Lwt.t

val del_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:string list ->
  Sql.db_t -> unit Lwt.t

val set_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:string list ->
  Sql.db_t -> unit Lwt.t











