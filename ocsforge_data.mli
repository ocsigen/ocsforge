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

(** @author Raphael Proust *)

(** {2 Database access with verification of permissions} *)




(** Add a task to the task tree.

    Parameters are :
    subject : the title of the message (a short description of the task)
    text : the content of the message (a long description of the task)
    length : the estimated time necessary to complete the task
    progress : the advancement of the task (must be <= 100)
    importance : the importance of the task (must be <= 100)
    kind : the type of the task (can be 'bug', 'rfe'...)
    area : the area where the task should be placed, let empty for an automatic
    set up, set to None to detach the task (create into a new area), set to
    [Some a] to create into area a

    Result is : the task identifier
    *)
val new_task :
  parent:Ocsforge_types.task ->
  subject:string ->
  text:string ->
  ?length:CalendarLib.Calendar.Period.t ->
  ?progress:int32 ->
  ?importance:int32 ->
  ?kind:string ->
  unit -> Ocsforge_types.task Lwt.t

val new_project :
  parent:Ocsforge_types.task ->
  name:string ->
  ?length:CalendarLib.Calendar.Period.t ->
  ?importance:int32 ->
  ?kind:string ->
  ?repository_kind:string ->
  ?repository_path:string ->
  ?wiki_container:string ->
  unit -> Ocsforge_types.task Lwt.t

(** Get the desired task.

   paramter is :
   the usuals server_paramsthe
   the task identifier
   result is : the task information (See module Ocsforge_types)
   *)
val get_task :
  task:Ocsforge_types.task -> Ocsforge_types.task_info Lwt.t


(** Get the history of a task
   [get_task_history id] result in the tuple [(info, history)] where info is
   the info about the task and history is the list of preivous states of the
   task. *)
val get_task_history :
  task:Ocsforge_types.task ->
  (Ocsforge_types.task_info * Ocsforge_types.task_history_info list) Lwt.t

val get_area :
  area:Ocsforge_types.right_area -> Ocsforge_types.right_area_info Lwt.t

val get_tree :
  root:Ocsforge_types.task ->
  ?with_deleted:bool ->
  ?depth:int ->
  unit -> Ocsforge_types.task_info Ocsforge_lang.Tree.tree Lwt.t

(** Get tasks with a specified parent.
   [get_sub_tasks parent] result in the list of desired tasks. *)
val get_sub_tasks :
  parent:Ocsforge_types.task -> Ocsforge_types.task_info list Lwt.t


val get_area_for_task :
  task:Ocsforge_types.task -> Ocsforge_types.right_area_info Lwt.t

val get_area_for_page :
  page:string -> Ocsforge_types.right_area_info Lwt.t



(** Get tasks sharing a common editor.
   [get_tasks_edited_by editor] result in the list of tasks with the
   [t_edit_author] field being [editor] *)
val get_tasks_edited_by :
  editor:User_sql.Types.userid -> Ocsforge_types.task_info list Lwt.t


(** Change fields for the task. Only use fields you are willing to change, other
   fields will be left untouched.*)
val edit_task :
  task:Ocsforge_types.task ->
  ?length:CalendarLib.Calendar.Period.t option ->
  ?progress:int32 option ->
  ?importance:int32 option ->
  ?kind:string option ->
  unit -> unit Lwt.t

(** Change fields for a right area. Only use fields you want to tamper with. *)
val edit_area :
  area:Ocsforge_types.right_area ->
  ?repository_path:string option ->
  ?repository_kind:string option ->
  ?version:string ->
  unit -> unit Lwt.t





(** Change a task parent and possibly area fields.

   Paramters are :
   task : the identifier of the task to move
   parent : the identifier of the new parent for the task
   area : if not specified, the area will be adapted to the new parent, if specified the area will
   take the given value.*)
val move_task :
  task:Ocsforge_types.task ->
  parent:Ocsforge_types.task ->
  unit Lwt.t

(** Detach a task into a new area.

   Parameters are :
   task : the task to detach
   parent : the new parent for the task. If unspecified, won't change parent.
   *)
val make_project :
  task:Ocsforge_types.task ->
  ?repository_kind:string ->
  ?repository_path:string ->
  unit -> unit Lwt.t



(** Getting kinds usable within the area *)
val get_kinds :
  area:Ocsforge_types.right_area ->
  string list Lwt.t

(** Adding, Deleting or Setting (aka wiping-out-then-adding) kinds.

   Parameters are :
   area : the area identifier
   kinds : a list of string that will be added/deleted/set
   in del_kinds kinds contain optional alternative values for deleted kinds. It
   is useful to avoid letting tasks with old kinds.
   *)
val add_kinds :
  area:Ocsforge_types.right_area ->
  kinds:string list -> unit Lwt.t

val del_kinds :
  area:Ocsforge_types.right_area ->
  kinds:(string * string option) list -> unit Lwt.t

val set_kinds :
  area:Ocsforge_types.right_area ->
  kinds:string list -> unit Lwt.t

val swap_kinds :
  area:Ocsforge_types.right_area ->
  kinds:(string * string) list -> unit Lwt.t

(** Getting a task's forum message content. *)
val find_subject :
  task:Ocsforge_types.task ->
  string Lwt.t


(** Getting separators in a subtree. The root of the subtree is the [~task]
    argument. *)
val get_separators :
  task:Ocsforge_types.task ->
  Ocsforge_types.separator_info list Lwt.t

(** Setting separator field. *)
val set_separator_content :
  separator:Ocsforge_types.separator ->
  content:string -> unit Lwt.t
val move_separator :
  separator:Ocsforge_types.separator ->
  after:Ocsforge_types.task -> unit Lwt.t

(** inserting a new separator *)
val insert_separator :
  after:Ocsforge_types.task ->
  content:string -> unit Lwt.t

