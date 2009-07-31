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

open CalendarLib

(* {2 Type conversion for database IO} *)

(** {3 Semi-abstract type for a task, a right tag and a task history} *)
type task_arg = [ `Task ]
type task = task_arg Opaque.int32_t
type right_area_arg = [ `Right_area ]
type right_area = right_area_arg Opaque.int32_t
type task_history_arg = [ `Task_history ]
type task_history = task_history_arg Opaque.int32_t 



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
  r_wikibox              : Wiki_types.wikibox ;
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
    r_wikibox              = Wiki_types.wikibox_of_sql wikibox;
 }


(** {3 For tasks} *)
type task_info = {
  t_id        : task;
  t_parent    : task;

  t_message : Forum_types.message;

  t_edit_author  : User_sql.Types.userid;
  t_edit_time    : Calendar.t;
  t_edit_version : string;

  t_length           : Calendar.Period.t option;
  t_progress         : int32 option;
  t_importance       : int32 option;
  t_deadline_time    : Date.t option;
  t_deadline_version : string option;
  t_kind             : string option;

  t_area             : right_area;

  t_tree_min         : int32;
  t_tree_max         : int32;

  t_deleted          : bool;
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
     Calendar.Period.t option * int32 option * int32 option
     * Calendar.t option * string option * string option *
     int32 * int32 * int32 * bool)

let get_task_info
      (id,  parent_id,
       message,
       edit_author, edit_time, edit_version,
       length,  progress,  importance,  deadline_time, deadline_version,  kind,
       area, tmin, tmax, deleted)
      = 
  {
    t_id     = task_of_sql id;
    t_parent = task_of_sql parent_id;

    t_message = Forum_types.message_of_sql message;

    t_edit_author  = User_sql.Types.userid_from_sql edit_author;
    t_edit_time    = edit_time;
    t_edit_version = edit_version;

    t_length           = length;
    t_progress         = progress;
    t_importance       = importance;
    t_deadline_time    = Ocsforge_lang.apply_on_opted
                           Calendar.to_date deadline_time;
    t_deadline_version = deadline_version;
    t_kind             = kind;

    t_area             = right_area_of_sql area;

    t_tree_max         = tmin;
    t_tree_min         = tmax;

    t_deleted          = deleted;
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
  th_deadline_time    : Date.t option;
  th_deadline_version : string option;
  th_kind             : string option;

  th_area             : right_area;
}

let task_history_of_sql (u : int32) = (Opaque.int32_t u : task_history)
let sql_of_task_history (u : task_history) = Opaque.t_int32 u

let task_history_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : task_history option)
let sql_of_task_history_option (u : task_history option) = Opaque.t_int32_option u

let string_of_task_history i = Int32.to_string (sql_of_task i)
let task_history_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

type raw_task_history_info =
    (int32 * int32 *
     int32 * Calendar.t * string *
     Calendar.Period.t option * int32 option * int32 option
     * Calendar.t option * string option * string option *
     int32)

let get_task_history_info
      (id, parent,
       editor, time, edit_version,
       length, progress, importance, deadline_time, deadline_ver, kind, 
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
    th_deadline_time    = Ocsforge_lang.apply_on_opted
                            Calendar.to_date deadline_time ;
    th_deadline_version = deadline_ver ;
    th_kind             = kind ;

    th_area             = right_area_of_sql right_zone ;
  }


module Tree =
struct

  type 'a tree = { content : 'a ; children : 'a tree list }
  exception Empty_tree

  let node n l = { content = n ; children = l }
  let get_content { content = n } = n
  let get_children { children = l } = l

  let iter f tree=
    let rec aux { content = t ; children = l } = f t l ; List.iter aux l in
      aux tree
  (** the function argument receives depth information *)
  let iteri f tree =
    let rec aux i { content = t ; children = l } =
      f t l i ;
        List.iter (aux (succ i)) l
    in aux 0 tree

  let find f tree =
    let rec aux { content = t ; children = l } =
      if f t l then node t l else auxaux l
    and auxaux = function
      | [] -> raise Not_found
      | hd::tl -> try aux hd with Not_found -> auxaux tl
    in aux tree

  let get_parent (tree : 'a tree) (n : 'a tree) : 'a tree =
    find (fun _ l -> List.mem n l) tree

  let get_depth (tree : 'a tree) (n : 'a tree) : int =
    let rec aux depth { content = t ; children = l } =
      if t = get_content n then depth else auxaux (succ depth) l
    and auxaux depth = function
      | [] -> raise Not_found
      | hd::tl -> try aux depth hd with Not_found -> auxaux (succ depth) tl
    in aux 0 tree

  let map f tree =
    let rec aux { content = t ; children = l } = 
      let (t,l) = f t l in
      let l = List.map aux l in
        node t l 
    in aux tree

  let filter f tree =
    let rec aux { content = t ; children = l } =
      if f t l then Some (node t  (Ocsforge_lang.filter_map aux l)) else None
    in
      Ocsforge_lang.unopt ~exc:Empty_tree (aux tree)

  let insert f tree n =
    map (fun t l -> (t, if f t l then n::l else l)) tree
  let insert_at tree n d =
    let d = get_content d in
    let rec aux { content = t ; children = l } =
      if t = d then node t (n :: l) else node t (auxaux [] l)
    and auxaux acc = function
      | [] -> List.rev acc
      | hd::tl -> auxaux ((aux hd) :: acc) tl
    in aux tree

  let move tree n d =
    insert_at (filter (fun nn _ -> nn <> n.content) tree) n d

  let to_list tree =
    let rec aux { content = t ; children = l } =
      t :: (List.flatten (List.map aux l))
    in aux tree

  let is_in_lineage parent child =
    let rec aux children =
      List.mem child children || auxaux children
    and auxaux = function
      | [] -> false
      | { children = c } :: tl -> aux c || auxaux tl
    in aux (get_children parent)

  let sort ?(comp = compare) t =
    let rec aux { content = n ; children = c } =
      node n (List.map aux (List.sort comp c))
    in aux t

end

module Alts =
struct

  let deadlines =
     (Ocsforge_lang.date_interval_list
        ~min:(Calendar.Date.today ())
        ~max:(Calendar.Date.add
                (Calendar.Date.today ())
                (Calendar.Date.Period.lmake ~day:7 ()))
        () )
    @(Ocsforge_lang.date_interval_list
        ~bump:(Calendar.Date.Period.lmake ~day:7 ())
        ~min:(Calendar.Date.add
                (Calendar.Date.today ())
                (Calendar.Date.Period.lmake ~day:7 ()))
        ~max:(Calendar.Date.add
                (Calendar.Date.today ())
                (Calendar.Date.Period.lmake ~month:1 ()))
        ()
     )

  let lengths =
    ( [Calendar.Period.lmake ~hour:1 () ;
       Calendar.Period.lmake ~hour:2 () ;
       Calendar.Period.lmake ~hour:3 () ;
       Calendar.Period.lmake ~hour:6 () ;
       Calendar.Period.lmake ~hour:12 ();
       Calendar.Period.lmake ~hour:24 ();]
     @(Ocsforge_lang.period_interval_list
         ~bump:(Calendar.Period.lmake ~hour:24 ())
         ~min:(Calendar.Period.lmake ~hour:48 ())
         ~max:(Calendar.Period.lmake ~hour:120 ())
         ())
    )

  let percents = Ocsforge_lang.int32_interval_list ~bump:(Int32.of_int 5)
                   ~min:Int32.zero ~max:(Int32.of_int 100) ()


end
