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




(** {5 abstract types for sql to/from ocaml safety} *)
type task_arg
type task = task_arg Opaque.int32_t
type right_area_arg
type right_area = right_area_arg Opaque.int32_t
type task_history_arg
type task_history = task_history_arg Opaque.int32_t




(** {2 For right areas...} *)


(** {5 info record} *)
type right_area_info = {
  r_id                   : right_area ;
  r_forum                : Forum_sql.Types.forum ;
  r_version              : string ;
  r_repository_kind      : string option ;
  r_repository_path      : string option ;
  r_wiki_container       : Wiki_types.wikibox option ;
  r_wiki                 : Wiki_types.wiki ;
}

(** {5 Conversion} *)
val right_area_of_sql : int32 -> right_area
val sql_of_right_area : right_area -> int32
val right_area_of_sql_option : int32 option -> right_area option
val sql_of_right_area_option : right_area option -> int32 option
val string_of_right_area : right_area -> string
val right_area_of_string : string -> right_area

(* NOT TO BE USED BUT IN ocsforge_sql *)
type raw_right_area_info = int32 * int32 * string * string option *
                           string option * int32 option * int32
val get_right_area_info : raw_right_area_info -> right_area_info


(** {2 For tasks...} *)

(** {5 info record} *)
type task_info = {
  t_id : task;
  t_parent : task;
  t_message : Forum_sql.Types.message;
  t_edit_author : User_sql.Types.userid;
  t_edit_time : CalendarLib.Calendar.t;
  t_edit_version : string;
  t_length : CalendarLib.Calendar.Period.t option;
  t_progress : int32 option;
  t_importance : int32 option;
  t_deadline_time : CalendarLib.Date.t option;
  t_deadline_version : string option;
  t_kind : string option;
  t_area : right_area;
  t_tree_min : int32;
  t_tree_max : int32;
  t_deleted          : bool;
}

(** {5 Conversion} *)
val task_of_sql : int32 -> task
val sql_of_task : task -> int32
val task_of_sql_option : int32 option -> task option
val sql_of_task_option : task option -> int32 option
val string_of_task : task -> string
val task_of_string : string -> task
val task_of_int : int -> task


(* NOT TO BE USED BUT IN ocsforge_sql *)
type raw_task_info =
    int32 * int32 * int32 * int32 * CalendarLib.Calendar.t * string *
    CalendarLib.Calendar.Period.t option * int32 option * int32 option *
    CalendarLib.Calendar.t option * string option * string option * int32 *
    int32 * int32 * bool
val get_task_info : raw_task_info -> task_info

(** {2 For task history marks...} *)


(** {5 info record} *)
type task_history_info = {
  th_id : task_history;
  th_parent : task;
  th_edit_author : User_sql.Types.userid;
  th_edit_time : CalendarLib.Calendar.t;
  th_edit_version : string;
  th_length : CalendarLib.Calendar.Period.t option;
  th_progress : int32 option;
  th_importance : int32 option;
  th_deadline_time : CalendarLib.Date.t option;
  th_deadline_version : string option;
  th_kind : string option;
  th_area : right_area;
}

(** {5 Conversion} *)
val task_history_of_sql : int32 -> task_history
val sql_of_task_history : task_history -> int32
val task_history_of_sql_option : int32 option -> task_history option
val sql_of_task_history_option : task_history option -> int32 option
val string_of_task_history : task -> string
val task_history_of_string : string -> task

(* NOT TO BE USED BUT IN ocsforge_sql *)
type raw_task_history_info =
    int32 * int32 * int32 * CalendarLib.Calendar.t * string *
    CalendarLib.Calendar.Period.t option * int32 option * int32 option *
    CalendarLib.Calendar.t option * string option * string option * int32
val get_task_history_info : raw_task_history_info -> task_history_info


module Tree :
sig
  type 'a tree = { content : 'a ; children : 'a tree list }
  exception Empty_tree

  val node : 'a -> 'a tree list -> 'a tree
  val get_content : 'a tree -> 'a
  val get_children : 'a tree -> 'a tree list

  val insert :
    ('a -> 'a tree list -> bool) -> 'a tree -> 'a tree -> 'a tree

  val filter : ('a -> 'a tree list -> bool) -> 'a tree -> 'a tree

  val sort :
    ?comp:('a tree -> 'a tree -> int) ->
    'a tree ->
    'a tree

end

module Alts :
sig
  val deadlines : CalendarLib.Date.t list
  val lengths : CalendarLib.Calendar.Period.t list
  val percents : int32 list
end
