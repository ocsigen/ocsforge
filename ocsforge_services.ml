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

(** Making services for a project.*)
let register_services_for_project ~name =
  let edit_task =
    Eliom_services.new_service
      ~sp
      ~path:(root_path::name::["edit_task"])
      ~get_params:
         (Params.int "id" **
          (Params.opt (Params.int "progress") **
           (Params.opt (Params.int "importance") **
            (Params.opt (Params.string "kind") **
             (Params.opt (Params.user_type
                            CalendarLib.Printer.Calendar.of_string
                            CalendarLib.Printer.Calendar.to_string
                            "deadline") **
              (Params.opt (Params.string "deadline_version") **
               (Params.opt (Params.int "length") **
                Params.opt (Params.bool "detach")
               )))))))
      ()
  in
  let add_task =
    Eliom_services.new_service
      ~sp
      ~path:(root_path::name::["new_task"])
      ~get_params:
         (Params.int "parent" **
          (Params.opt (Params.int "progress") **
           (Params.opt (Params.int "importance") **
            (Params.opt (Params.string "kind") **
             (Params.opt (Params.user_type
                            CalendarLib.Printer.Calendar.of_string
                            CalendarLib.Printer.Calendar.to_string
                            "deadline") **
              (Params.opt (Params.string "deadline_version") **
               (Params.opt (Params.int "length") **
                (Params.opt (Params.bool "detach")
                ))))))))
      ()
  in

    Eliom_predefmod.Action.register
      ~service:add_task
      (fun sp (parent, (progress, (importance, (kind,
               (deadline_time, (deadline_version, (length, detach))))))) () ->
         let parent = Types.task_of_int parent in
         let length = CalendarLib.Calendar.Peridod.lmake ~hour:length () in
           match detach with
             | Some true ->
               Data.new_task ~sp ~parent
                 ?length ?progress ?importance
                 ?deadline_time ?deadline_version ?kind ~area:None ()
             | None | Some false ->
               Data.new_task ~sp ~parent
                 ?length ?progress ?importance
                 ?deadline_time ?deadline_version ?kind ()
      );

    Eliom_predefmod.Action.register
      ~service:edit_task
      (fun sp (id, (progress, (importance, (kind,
               (deadline_time, (deadline_version, (length))))))) () ->
         let task = Types.task_of_int id in
         let length = CalendarLib.Calendar.Peridod.lmake ~hour:length () in
         Data.new_task ~sp ~task
           ?length ?progress ?importance
           ?deadline_time ?deadline_version ?kind ());



  (* returning the services *)
  (edit_task, add_task)

