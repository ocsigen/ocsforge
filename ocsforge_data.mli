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



(** {2 Database access with verification of permissions} *)




(** Add a task to the task tree.
  *
  * Parameters are :
  * sp : the usual session parameters
  * subject : the title of the message (a short description of the task)
  * text : the content of the message (a long description of the task)
  * length : the estimated time necessary to complete the task
  * progress : the advancement of the task (must be <= 100)
  * importance : the importance of the task (must be <= 100)
  * deadline_time : the deadline for task completion
  * deadline_version : the version the task must be completed for
  * kind : the type of the task (can be 'bug', 'rfe'...)
  * area : the area where the task should be placed, let empty for an automatic
  * set up, set to None to detach the task (create into a new area), set to
  * [Some a] to create into area a
  *
  * Result is : the task identifier
  * *)
val new_task :
  sp:Eliom_sessions.server_params ->
  parent:Ocsforge_types.task ->
  subject:string ->
  text:string ->
  ?length:CalendarLib.Calendar.Period.t ->
  ?progress:int32 ->
  ?importance:int32 ->
  ?deadline_time:CalendarLib.Calendar.t ->
  ?deadline_version:string ->
  ?kind:string ->
  ?area:Ocsforge_types.right_area option -> unit -> Ocsforge_types.task Lwt.t


(** Get the desired task.
  *
  * paramter is :
  * the usuals server_paramsthe
  * the task identifier
  * result is : the task information (See module Ocsforge_types)
  * *)
val get_task :
  sp:Eliom_sessions.server_params ->
  task:Ocsforge_types.task -> Ocsforge_types.task_info Lwt.t


(** Get the history of a task
  * [get_task_history sp id] result in the tuple [(info, history)] where info is
  * the info about the task and history is the list of preivous states of the
  * task. *)
val get_task_history :
  sp:Eliom_sessions.server_params ->
  task:Ocsforge_types.task ->
  (Ocsforge_types.task_info * Ocsforge_types.task_history_info list) Lwt.t


(** Get tasks with a specified parent.
  * [get_sub_tasks sp parent] result in the list of desired tasks. *)
val get_sub_tasks :
  sp:Eliom_sessions.server_params ->
  parent:Ocsforge_types.task -> Ocsforge_types.task_info list Lwt.t


(** Get tasks sharing a common editor.
  * [get_tasks_edited_by sp editor] result in the list of tasks with the
  * [t_edit_author] field being [editor] *)
val get_tasks_edited_by :
  sp:Eliom_sessions.server_params ->
  editor:User_sql.Types.userid -> Ocsforge_types.task_info list Lwt.t


(** Change fields for the task. Only use fields you are willing to change, other
  * fields will be left untouched.*) 
val edit_task :
  sp:Eliom_sessions.server_params ->
  task:Ocsforge_types.task ->
  ?length:CalendarLib.Calendar.Period.t ->
  ?progress:int32 ->
  ?importance:int32 ->
  ?deadline_time:CalendarLib.Calendar.t ->
  ?deadline_version:string -> ?kind:string -> unit -> unit Lwt.t


(** Change a task parent and possibly area fields.
*
* Paramters are :
* task : the identifier of the task to move
* parent : the identifier of the new parent for the task
* area : if not specified, the area will be adapted to the new parent, if specified the area will
* take the given value.*)
val move_task :
  sp:Eliom_sessions.server_params ->
  task:Ocsforge_types.task ->
  parent:Ocsforge_types.task ->
  ?area:Ocsforge_types.right_area ->
  unit -> unit Lwt.t

(** Detach a task into a new area.
  *
  * Parameters are :
  * sp : the usual
  * task : the task to detach
  * parent : the new parent for the task. If unspecified, won't change parent.
  * *)
val detach_task :
  sp:Eliom_sessions.server_params ->
  task:Ocsforge_types.task ->
  ?parent:Ocsforge_types.task ->
  unit -> unit Lwt.t

