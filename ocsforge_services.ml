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

let ( ** ) = Eliom_parameters.prod
let ( >>= ) = Lwt.bind

module Params = Eliom_parameters
module Data = Ocsforge_data
module Types = Ocsforge_types

open CalendarLib




let set_length_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Ocsforge_lang.period_of_string)
          (Ocsforge_lang.string_of_t_opt Ocsforge_lang.string_of_period)
          "length")
      )
    (* error_handler *)
    (fun sp () (task, length) ->
       Data.edit_task ~sp ~task ~length ())

let set_progress_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Int32.of_string)
          (Ocsforge_lang.string_of_t_opt Int32.to_string)
          "progress")
      )
    (* error_handler *)
    (fun sp () (task, progress) ->
       Data.edit_task ~sp ~task ~progress ())

let set_importance_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Int32.of_string)
          (Ocsforge_lang.string_of_t_opt Int32.to_string)
          "importance")
      )
    (* error_handler *)
    (fun sp () (task, importance) ->
       Data.edit_task ~sp ~task ~importance ())

let set_deadline_time_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string Printer.Calendar.from_string)
          (Ocsforge_lang.string_of_t_opt Printer.Calendar.to_string)
          "deadline_t")
      )
    (* error_handler *)
    (fun sp () (task, deadline_time) ->
       Data.edit_task ~sp ~task ~deadline_time ())

let set_deadline_version_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.string "deadline_v")
      )
    (* error_handler *)
    (fun sp () (task, dlv) ->
       Data.edit_task ~sp ~task
         ~deadline_version:(if dlv = "" then None else Some dlv) ())

let set_kind_service =
  Eliom_predefmod.Action.register_new_post_coservice'
    ~options:`NoReload
    ~post_params:(
       (Params.user_type
          Types.task_of_string
          Types.string_of_task
          "id") **
       (Params.user_type
          (Ocsforge_lang.t_opt_of_string (fun s -> s))
          (Ocsforge_lang.string_of_t_opt (fun s -> s))
          "kind")
      )
    (* error_handler *)
    (fun sp () (task, kind) ->
       Data.edit_task ~sp ~task ~kind ())

