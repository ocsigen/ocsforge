
let apply_on_opted f = function
  | None   -> None
  | Some v -> Some (f v)


(* {2 Type conversion for database IO} *)

(** {3 Semi-abstract type for a task, a right tag and a task history} *)
type task_arg = [ `Task ]
type task = task_arg Opaque.int32_t
type right_area_arg = [ `Right_area ]
type right_area = right_area_arg Opaque.int32_t
type task_history_arg = [ `Task_history ]
type task_history = task_history_arg Opaque.int32_t


(** Needed intermediary types. *)
exception Not_a_percent
type percent = int32 (*TODO : hide type*)
let percent_of_int32 (n : int32) =
  if ( let m = Int32.to_int n in 100>=m && m>=0 )
  then (n : percent)
  else raise Not_a_percent
let int32_of_percent (n : percent) = (n : int32) 


(** {3 For right tags : right management for project tree} *)
type right_area_info = {
  r_id      : right_area;
  r_forum   : Forum_sql.Types.forum;
  r_version : string;
}

let right_area_of_sql (u : int32) = (Opaque.int32_t u : right_area)
let sql_of_right_area (u : right_area) = Opaque.t_int32 u

let right_area_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : right_area option)
let sql_of_right_area_option (u : right_area option) = Opaque.t_int32_option u

let string_of_right_area i = Int32.to_string (sql_of_right_area i)
let right_area_of_string s = (Opaque.int32_t (Int32.of_string s) : right_area)

type raw_right_area_info =
    (int32 * int32)

let get_right_area_info (id, forum_id, ver) =
  {
    r_id      = right_area_of_sql id;
    r_forum   = Forum_sql.Types.forum_of_sql forum_id;
    r_version = ver
  }


(** {3 For tasks} *)
type task_info = {
  t_id        : task;
  t_parent    : task;

  t_message : Forum_sql.Types.message;

  t_edit_author  : User_sql.Types.userid;
  t_edit_time    : CalendarLib.Calendar.t;
  t_edit_version : string;

  t_length           : Calendar.Period.t option;
  t_progress         : percent option;
  t_importance       : percent;
  t_deadline_time    : CalendarLib.Calendar.t option;
  t_deadline_version : string option;
  t_kind             : string;

  t_area             : right_area;
  t_area_inheritance : right_area;
}



let task_of_sql (u : int32) = (Opaque.int32_t u : task)
let sql_of_task (u : task) = Opaque.t_int32 u

let task_of_sql_option (u : int32 option) = 
  (Opaque.int32_t_option u : task option)
let sql_of_task_option (u : task option) = Opaque.t_int32_option u

let string_of_task i = Int32.to_string (sql_of_task i)
let task_of_string s = (Opaque.int32_t (Int32.of_string s) : task)

type raw_task_info =
    (int32 * int32 *
     int32 *
     int32 * CalendarLib.Calendar.t * string *
     CalendarLib.Period.t option * int32 option * int32
     * CalendarLib.Calendar.t option * string option * string *
     int32 * int32)

let get_task_info
      (id,  parent_id,
       message,
       edit_author, edit_time, edit_version,
       length,  progress,  importance,  deadline_time, deadline_version,  kind,
       area,  area_inheritance)
      = 
  {
    t_id        = task_of_sql id;
    t_parent_id = task_of_sql parent_id;

    t_message = Forum_sql.Types.message_of_sql message;

    t_edit_author  = User_sql.Types.userid_from_sql editor_id;
    t_edit_time    = datetime_edit;
    t_edit_version = edit_version;

    t_length           = length;
    t_progress         = aplly_on_opted percent_of_int32 progress;
    t_importance       = percent_of_int32 importance;
    t_deadline_time    = deadline_time;
    t_deadline_version = deadline_version;
    t_kind             = task_kind_of_string kind;

    t_area             = right_area_of_sql right_tag;
    t_area_inheritance = right_area_of_sql right_inheritance;
  }


(** {3 For tasks history} *)
type task_history_info = {
  th_id        : task;
  th_parent_id : task;

  th_edit_author  : User_sql.Types.userid;
  th_edit_time    : CalendarLib.Calendar.t;
  th_edit_version : string;

  th_length           : Calendar.Period.t option;
  th_progress         : percent option;
  th_importance       : percent;
  th_deadline_time    : CalendarLib.Calendar.t option;
  th_deadline_version : string option;
  th_kind             : string;

  th_area             : right_area;
  th_area_inheritance : right_area;
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
     int32 * CalendarLib.Calendar.t * string *
     CalendarLib.Period.t option * int32 option * int32
     * CalendarLib.Calendar.t option * string option * string *
     int32 * int32)

let get_task_history_info
      (id, parent,
       editor, time, edit_version,
       length, progress, importance, deadline_time, deadline_ver, kind, 
       right_zone, right_inheritance) =
  {
    th_id        = task_history_of_sql id ;
    th_parent_id = task_of_sql parent ;

    th_edit_author  = User_sql.Types.userid_from_sql editor ;
    th_edit_time    = datetime ;
    th_edit_version = edit_version ;

    th_length           = length ;
    th_progress         = apply_on_opted percent_of_int32 progress ;
    th_importance       = percent_of_int32 importance ;
    th_deadline_time    = deadline_time ;
    th_deadline_version = deadline_ver ;
    th_kind             = kind_of_string kind ;

    th_area             = right_area_of_sql right_zone ;
    th_area_inheritance = right_area_of_sql right_inheritance ;
  }


