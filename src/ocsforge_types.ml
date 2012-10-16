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
  < forum_id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  repository_kind : < get : unit; nul : Sql.nullable;
  t : Sql.string_t >
    Sql.t;
  repository_path : < get : unit; nul : Sql.nullable;
  t : Sql.string_t >
    Sql.t;
  root_task : < get : unit; nul : Sql.nullable; t : Sql.int32_t >
    Sql.t;
  sources_container : < get : unit; nul : Sql.non_nullable;
  t : Sql.int32_t >
    Sql.t;
  version : < get : unit; nul : Sql.non_nullable; t : Sql.string_t >
    Sql.t;
  wiki : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  wiki_container : < get : unit; nul : Sql.nullable;
  t : Sql.int32_t >
    Sql.t >

let get_right_area_info sql_data =
  let id = Sql.get sql_data#id
  and forum_id = Sql.get sql_data#forum_id
  and ver = Sql.get sql_data#version
  and kind = Sql.getn sql_data#repository_kind
  and path = Sql.getn sql_data#repository_path
  and task = Sql.getn sql_data#root_task
  and cont = Sql.getn sql_data#wiki_container
  and wik = Sql.get sql_data#wiki
  and wikibox = Sql.get sql_data#sources_container in
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
  < area : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  area_root : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t >
    Sql.t;
  deleted : < get : unit; nul : Sql.non_nullable; t : Sql.bool_t >
    Sql.t;
  edit_author : < get : unit; nul : Sql.non_nullable;
  t : Sql.int32_t >
    Sql.t;
  edit_time : < get : unit; nul : Sql.non_nullable;
  t : Sql.timestamp_t >
    Sql.t;
  edit_version : < get : unit; nul : Sql.non_nullable;
  t : Sql.string_t >
    Sql.t;
  id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  importance : < get : unit; nul : Sql.nullable; t : Sql.int32_t >
    Sql.t;
  kind : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t;
  length : < get : unit; nul : Sql.nullable; t : Sql.interval_t >
    Sql.t;
  message : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  parent : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  progress : < get : unit; nul : Sql.nullable; t : Sql.int32_t >
    Sql.t;
  tree_max : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  tree_min : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t >

let get_task_info sql_data =
  let id = Sql.get sql_data#id
  and parent_id = Sql.get sql_data#parent
  and message = Sql.get sql_data#message
  and edit_author = Sql.get sql_data#edit_author
  and edit_time = Sql.get sql_data#edit_time
  and edit_version = Sql.get sql_data#edit_version
  and length = Sql.getn sql_data#length
  and progress = Sql.getn sql_data#progress
  and importance = Sql.getn sql_data#importance
  and kind = Sql.getn sql_data#kind
  and area = Sql.get sql_data#area
  and tmin = Sql.get sql_data#tree_min
  and tmax = Sql.get sql_data#tree_max
  and deleted = Sql.get sql_data#deleted
  and root = Sql.get sql_data#area_root in
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
  < area : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  edit_author : < get : unit; nul : Sql.non_nullable;
  t : Sql.int32_t >
    Sql.t;
  edit_time : < get : unit; nul : Sql.non_nullable;
  t : Sql.timestamp_t >
    Sql.t;
  edit_version : < get : unit; nul : Sql.non_nullable;
  t : Sql.string_t >
    Sql.t;
  id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  importance : < get : unit; nul : Sql.nullable; t : Sql.int32_t >
    Sql.t;
  kind : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t;
  length : < get : unit; nul : Sql.nullable; t : Sql.interval_t >
    Sql.t;
  parent : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  progress : < get : unit; nul : Sql.nullable; t : Sql.int32_t >
    Sql.t >

let get_task_history_info sql_data =
  let id = Sql.get sql_data#id
  and parent = Sql.get sql_data#parent
  and editor = Sql.get sql_data#edit_author
  and time = Sql.get sql_data#edit_time
  and edit_version = Sql.get sql_data#edit_version
  and length = Sql.getn sql_data#length
  and progress = Sql.getn sql_data#progress
  and importance = Sql.getn sql_data#importance
  and kind = Sql.getn sql_data#kind
  and right_zone = Sql.get sql_data#area in
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

type raw_separator_info =
  < after : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t >
    Sql.t;
  content : < get : unit; nul : Sql.non_nullable; t : Sql.string_t >
    Sql.t;
  id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t >
let get_separator_info sql_data =
  let id = Sql.get sql_data#id
  and content = Sql.get sql_data#content
  and after = Sql.get sql_data#after in
  {
    s_id = separator_of_sql id ;
    s_content = content ;
    s_after = task_of_sql after ;
  }
