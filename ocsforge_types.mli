

(*misc functions*)
val apply_on_opted : ('a -> 'b) -> 'a option -> 'b option
val interval_list : ?bump:int -> min:int -> max:int -> int list


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
  r_id          : right_area ;
  r_forum       : Forum_sql.Types.forum ;
  r_version     : string ;
  r_inheritance : right_area ;
}

(** {5 Conversion} *)
val right_area_of_sql : int32 -> right_area
val sql_of_right_area : right_area -> int32
val right_area_of_sql_option : int32 option -> right_area option
val sql_of_right_area_option : right_area option -> int32 option
val string_of_right_area : right_area -> string
val right_area_of_string : string -> right_area

(* NOT TO BE USED BUT IN ocsforge_sql *)
type raw_right_area_info = int32 * int32 * string * int32
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
  t_deadline_time : CalendarLib.Calendar.t option;
  t_deadline_version : string option;
  t_kind : string;
  t_area : right_area;
}

(** {5 Conversion} *)
val task_of_sql : int32 -> task
val sql_of_task : task -> int32
val task_of_sql_option : int32 option -> task option
val sql_of_task_option : task option -> int32 option
val string_of_task : task -> string
val task_of_string : string -> task

(* NOT TO BE USED BUT IN ocsforge_sql *)
type raw_task_info =
    int32 * int32 * int32 * int32 * CalendarLib.Calendar.t * string *
    CalendarLib.Calendar.Period.t option * int32 option * int32 option *
    CalendarLib.Calendar.t option * string option * string * int32
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
  th_deadline_time : CalendarLib.Calendar.t option;
  th_deadline_version : string option;
  th_kind : string;
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
    CalendarLib.Calendar.t option * string option * string * int32
val get_task_history_info : raw_task_history_info -> task_history_info

