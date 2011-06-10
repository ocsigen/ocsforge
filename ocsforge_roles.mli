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

(** Groups for task and area related rights *)
val task_admin :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_property_editor :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_message_editor :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_message_editor_if_author :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_moderator :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_sticky_setter :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_deletor :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_deletor_if_author :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_writer :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_writer_not_moderated :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_comment_reader :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_reader :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_creator :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_mover :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_mover_from :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val task_mover_to :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val subarea_creator :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val kinds_setter :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val version_setter :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group
val repository_setter :
  Ocsforge_types.right_area_arg User_sql.Types.parameterized_group

(** Role for a user *)
type role = {
  task_reader                        : bool Lwt.t Lazy.t ;
  task_comment_reader                : bool Lwt.t Lazy.t ;
  task_comment_moderator             : bool Lwt.t Lazy.t ;
  task_comment_sticky_setter         : bool Lwt.t Lazy.t ;
  task_comment_deletor               : bool Lwt.t Lazy.t ;
  task_comment_writer                : bool Lwt.t Lazy.t ;
  task_comment_writer_not_moderated  : bool Lwt.t Lazy.t ;
  task_property_editor               : bool Lwt.t Lazy.t ;
  task_message_editor                : bool Lwt.t Lazy.t ;
  task_message_editor_if_author      : bool Lwt.t Lazy.t ;
  task_admin                         : bool Lwt.t Lazy.t ;
  task_creator                       : bool Lwt.t Lazy.t ;
  task_mover                         : bool Lwt.t Lazy.t ;
  task_mover_from                    : bool Lwt.t Lazy.t ;
  task_mover_to                      : bool Lwt.t Lazy.t ;
  subarea_creator                    : bool Lwt.t Lazy.t ;
  kinds_setter                       : bool Lwt.t Lazy.t ;
  version_setter                     : bool Lwt.t Lazy.t ;
  repository_setter                  : bool Lwt.t Lazy.t ;
}

(** Low level and not cached ! DO NOT USE *)
val get_role : area:Ocsforge_types.right_area -> role Lwt.t
val blunt_sd : unit -> Ocsforge_types.right_area -> role Lwt.t
type area_sd = Ocsforge_types.right_area -> role Lwt.t
val area_key : area_sd Polytables.key
val get_area_session_data : unit -> area_sd


(** Get the role of a user. This version uses cache ! *)
val get_area_role : Ocsforge_types.right_area -> role Lwt.t
