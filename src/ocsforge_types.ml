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


open CalendarLib

(* {2 Type conversion for database IO} *)

(** {3 Semi-abstract type for a task, a right tag and a task history} *)
type task_arg = [ `Task ]
type task = task_arg Opaque.int32_t
type right_area_arg = [ `Right_area ]
type right_area = right_area_arg Opaque.int32_t
type task_history_arg = [ `Task_history ]
type task_history = task_history_arg Opaque.int32_t 
type separator_arg = [ `Separator ]
type separator = separator_arg Opaque.int32_t

(** {3 For right tags : right management for project tree} *)
type right_area_info = {
  r_id                   : right_area ;
  r_forum                : Forum_types.forum ;
  r_version              : string ;
  r_repository_kind      : string option ;
  r_repository_path      : string option ;
  r_root_task            : task option ; 
  r_wiki_container       : Wiki_types.wikibox option ;
  r_wiki                 : Wiki_types.wiki ;
  r_sources_container    : Wiki_types.wikibox ;
}

let right_area_of_sql (u : int32) = (Opaque.int32_t u : right_area)
let sql_of_right_area (u : right_area) = Opaque.t_int32 u

let task_of_sql (u : int32) = (Opaque.int32_t u : task)
let sql_of_task (u : task) = Opaque.t_int32 u

let right_area_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : right_area option)
let sql_of_right_area_option (u : right_area option) = Opaque.t_int32_option u

let string_of_right_area i = Int32.to_string (sql_of_right_area i)
let right_area_of_string s = (Opaque.int32_t (Int32.of_string s) : right_area)

type raw_right_area_info =
    (int32 * int32 * string *
     string option * string option * int32 option * int32 option * int32 * int32)

let get_right_area_info (id, forum_id, ver, kind, path, task, cont, wik, wikibox) =
  {
    r_id                   = right_area_of_sql id ;
    r_forum                = Forum_types.forum_of_sql forum_id ;
    r_version              = ver ;
    r_repository_kind      = kind ;
    r_repository_path      = path ;
    r_root_task            = Ocsforge_lang.apply_on_opted 
                                task_of_sql task ;
    r_wiki_container       = Ocsforge_lang.apply_on_opted
                               Wiki_types.wikibox_of_sql cont ;
    r_wiki                 = Wiki_types.wiki_of_sql wik ;
    r_sources_container    = Wiki_types.wikibox_of_sql wikibox;
 }


(** {3 For tasks} *)
type task_info = {
  t_id        : task ;
  t_parent    : task ;

  t_message : Forum_types.message ;

  t_edit_author  : User_sql.Types.userid ;
  t_edit_time    : Calendar.t ;
  t_edit_version : string ;

  t_length           : Calendar.Period.t option;
  t_progress         : int32 option  ;
  t_importance       : int32 option  ;
  t_kind             : string option ;

  t_area             : right_area ;

  t_tree_min         : int32 ;
  t_tree_max         : int32 ;

  t_deleted          : bool ;
  t_area_root        : bool ;

}




let task_of_int (u : int) : task = Opaque.int32_t (Int32.of_int u)

let task_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : task option)
let sql_of_task_option (u : task option) = Opaque.t_int32_option u

let string_of_task i = Int32.to_string (sql_of_task i)
let task_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

type raw_task_info =
    (int32 * int32 *
     int32 *
     int32 * Calendar.t * string *
     Calendar.Period.t option * int32 option * int32 option * string option *
     int32 * int32 * int32 * bool * bool)

let get_task_info
      (id,  parent_id,
       message,
       edit_author, edit_time, edit_version,
       length,  progress,  importance, kind,
       area, tmin, tmax, deleted, root)
      = 
  {
    t_id     = task_of_sql id ;
    t_parent = task_of_sql parent_id ;

    t_message = Forum_types.message_of_sql message ;

    t_edit_author  = User_sql.Types.userid_from_sql edit_author ;
    t_edit_time    = edit_time ;
    t_edit_version = edit_version ;

    t_length           = length ;
    t_progress         = progress ;
    t_importance       = importance ;
    t_kind             = kind ;

    t_area             = right_area_of_sql area ;

    t_tree_max         = tmin ;
    t_tree_min         = tmax ;

    t_deleted          = deleted ;
    t_area_root        = root ;
  }


(** {3 For tasks history} *)
type task_history_info = {
  th_id     : task_history;
  th_parent : task;

  th_edit_author  : User_sql.Types.userid;
  th_edit_time    : Calendar.t;
  th_edit_version : string;

  th_length           : Calendar.Period.t option;
  th_progress         : int32 option;
  th_importance       : int32 option;
  th_kind             : string option;

  th_area             : right_area;
}

let task_history_of_sql (u : int32) = (Opaque.int32_t u : task_history)
let sql_of_task_history (u : task_history) = Opaque.t_int32 u

let task_history_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : task_history option)
let sql_of_task_history_option (u : task_history option) =
  Opaque.t_int32_option u

let string_of_task_history i = Int32.to_string (sql_of_task i)
let task_history_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

type raw_task_history_info =
    (int32 * int32 *
     int32 * Calendar.t * string *
     Calendar.Period.t option * int32 option * int32 option * string option *
     int32)

let get_task_history_info
      (id, parent,
       editor, time, edit_version,
       length, progress, importance, kind, 
       right_zone) =
  {
    th_id     = task_history_of_sql id ;
    th_parent = task_of_sql parent ;

    th_edit_author  = User_sql.Types.userid_from_sql editor ;
    th_edit_time    = time ;
    th_edit_version = edit_version ;

    th_length           = length ;
    th_progress         = progress ;
    th_importance       = importance ;
    th_kind             = kind ;

    th_area             = right_area_of_sql right_zone ;
  }

type separator_info =
    { s_id : separator ; s_after : task ; s_content : string ; }

let separator_of_sql ( u : int32 ) = ( Opaque.int32_t u : separator )
let sql_of_separator ( u : separator ) = ( Opaque.t_int32 u : int32 )
let string_of_separator s = Int32.to_string (sql_of_separator s)
let separator_of_string s = separator_of_sql (Int32.of_string s)

type raw_separator_info = int32 * int32 * string
let get_separator_info (id, after, content) =
  {
    s_id = separator_of_sql id ;
    s_content = content ;
    s_after = task_of_sql after ;
  }
