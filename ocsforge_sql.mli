(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Universit� Paris Diderot - CNRS
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
  ?deadline_time:CalendarLib.Date.t ->
  ?deadline_version:string ->
  ?kind:string ->
  area:Ocsforge_types.right_area ->
  ?area_root:bool ->
  unit -> Ocsforge_types.task Lwt.t

(**Insert a new area in the database.
* needs a forum to be associated to
* can take optionnaly an identifier and a version string
* BEWARE : id must be unique, use [next_right_area_id]*)
val new_area :
  ?id:Ocsforge_types.right_area ->
  forum:Forum_sql.Types.forum ->
  ?version:string ->
  ?repository_kind:string ->
  ?repository_path:string ->
  ?wiki_container:Wiki_types.wikibox ->
  wiki:Wiki_types.wiki ->
  unit -> Ocsforge_types.right_area Lwt.t

val get_path_for_area :
  area:Ocsforge_types.right_area ->
  Sql.db_t -> string list option Lwt.t


(** Get the nextval of the right_area_id_seq.
  * Used when detaching a task into a new area*)
val next_right_area_id :
  Sql.db_t -> Ocsforge_types.right_area Lwt.t

(** Get the nextval of the task_id_seq. For bootstraping purpose*)
val next_task_id :
  Sql.db_t -> Ocsforge_types.task Lwt.t


(** Get the task information based on identifier*)
val get_task_by_id :
  task_id:Ocsforge_types.task ->
  ?with_deleted:bool ->
  Sql.db_t -> Ocsforge_types.task_info Lwt.t

(** Get the task history entries.*)
val get_task_history_by_id :
  task_id:Ocsforge_types.task ->
  Sql.db_t -> (Ocsforge_types.task_info * Ocsforge_types.task_history_info list) Lwt.t

(** Get every task sharing the same specified parent*)
val get_tasks_by_parent :
  parent:Ocsforge_types.task ->
  ?with_deleted:bool ->
  Sql.db_t -> Ocsforge_types.task_info list Lwt.t

(** Get every task in a subtree*)
val get_tasks_in_tree :
  root:Ocsforge_types.task ->
  ?with_deleted:bool -> unit ->
  Sql.db_t -> Ocsforge_types.task_info list Lwt.t




(** Get every task having the given editor.*)
val get_tasks_by_editor :
  editor:User_sql.Types.userid ->
  ?with_deleted:bool -> unit ->
  Sql.db_t -> Ocsforge_types.task_info list Lwt.t

(** Get the area a task is in*)
val get_area_for_task :
  task_id:Ocsforge_types.task ->
  Sql.db_t -> Ocsforge_types.right_area Lwt.t

(** Get the info for the area the task is in *)
val get_area_info_for_task :
  task_id:Ocsforge_types.task ->
  Sql.db_t -> Ocsforge_types.right_area_info Lwt.t


(** Get area info about a task*)
val get_area_by_id :
  area_id:Ocsforge_types.right_area ->
  Sql.db_t -> Ocsforge_types.right_area_info Lwt.t

(** Get the version for the area *)
val get_area_version :
  area_id:Ocsforge_types.right_area ->
  Sql.db_t -> string Lwt.t

(** Make a history entry for the task.
  * To be used only when editing.*)
val copy_in_history :
  task_id:Ocsforge_types.task ->
  Sql.db_t -> unit Lwt.t

(** Change the edit_* fields of the task. Useful when editing*)
val stamp_edition :
  task_id:Ocsforge_types.task ->
  author:User_sql.Types.userid ->
  Sql.db_t -> unit Lwt.t

(** Set fields values for a given task.*)

val set_length :
  task_id:Ocsforge_types.task ->
  length:CalendarLib.Calendar.Period.t option ->
  Sql.db_t -> unit Lwt.t
val set_progress :
  task_id:Ocsforge_types.task ->
  progress:int32 option ->
  Sql.db_t -> unit Lwt.t
val set_importance :
  task_id:Ocsforge_types.task ->
  importance:int32 option ->
  Sql.db_t -> unit Lwt.t
val set_deadline_time :
  task_id:Ocsforge_types.task ->
  deadline_time:CalendarLib.Date.t option ->
  Sql.db_t -> unit Lwt.t
val set_deadline_version :
  task_id:Ocsforge_types.task ->
  deadline_version:string option ->
  Sql.db_t -> unit Lwt.t
val set_kind :
  task_id:Ocsforge_types.task ->
  kind:string option ->
  Sql.db_t -> unit Lwt.t
val set_area :
  task_id:Ocsforge_types.task ->
  area:Ocsforge_types.right_area ->
  Sql.db_t -> unit Lwt.t
val set_parent :
  task_id:Ocsforge_types.task ->
  parent:Ocsforge_types.task ->
  Sql.db_t -> unit Lwt.t
val set_deleted :
  task_id:Ocsforge_types.task ->
  deleted:bool ->
  Sql.db_t -> unit Lwt.t


(** When moving a task to a new parent, updates tree_min and tree_max fields. *)
val change_tree_marks :
  task_id:Ocsforge_types.task ->
  parent_id:Ocsforge_types.task -> Sql.db_t -> unit Lwt.t


(** Add/Delete/Set the kinds that can be associated to a task in the specified
  * area.
* the delete function can uses a tuple of list with the second being an
* optionnal alternative for kinds.
*
* eg : del ~area ~kinds:(["BUG";"RFE";"RFF"],[Some "Bug", Some "Feature", None]) db
* replaces instances of "BUG" with "Bug" and "RFE" with "Feature"*)
val get_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  Sql.db_t -> string list Lwt.t



val add_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:string list ->
  Sql.db_t -> unit Lwt.t

val del_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:(string * string option) list ->
  Sql.db_t -> unit Lwt.t

val set_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:string list ->
  Sql.db_t -> unit Lwt.t

val swap_kinds_for_area :
  area_id:Ocsforge_types.right_area ->
  kinds:(string * string) list ->
  Sql.db_t -> unit Lwt.t

val is_area_root :
  task:Ocsforge_types.task ->
  Sql.db_t -> bool Lwt.t

val get_projects_path_list :
  unit ->
  (string option * int32 option) list Lwt.t


(**/**)
val bootstrap_task :
  area:Ocsforge_types.right_area ->
  message:Forum_sql.Types.message ->
  unit Lwt.t
val get_task_count :
  unit -> int Lwt.t


