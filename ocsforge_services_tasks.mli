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


val nl_param :
  (Ocsforge_types.task * (string * bool), [ `WithoutSuffix ],
   ([ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    [ `One of bool ] Eliom_parameters.param_name)))
  Eliom_parameters.non_localized_params



val non_localized_service :
  (unit * (Ocsforge_types.task * (string * bool)), unit,
   [> `Nonattached of 'a Eliom_services.na_s ], [ `WithoutSuffix ],
   unit *
   ([ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
    ([ `One of string ] Eliom_parameters.param_name *
     [ `One of bool ] Eliom_parameters.param_name)),
   unit, [> `Unregistrable ])
  Eliom_services.service



val set_length_service :
  (unit, Ocsforge_types.task * CalendarLib.Calendar.Period.t option,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of CalendarLib.Calendar.Period.t option ] Eliom_parameters.param_name,
   [> `Registrable ])
  Eliom_services.service



val set_progress_service :
  (unit, Ocsforge_types.task * int32 option,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of int32 option ] Eliom_parameters.param_name, [> `Registrable ])
  Eliom_services.service



val set_importance_service :
  (unit, Ocsforge_types.task * int32 option,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of int32 option ] Eliom_parameters.param_name, [> `Registrable ])
  Eliom_services.service



val set_deadline_time_service :
  (unit, Ocsforge_types.task * CalendarLib.Printer.Date.t option,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of CalendarLib.Printer.Date.t option ] Eliom_parameters.param_name,
   [> `Registrable ])
  Eliom_services.service



val set_deadline_version_service :
  (unit, Ocsforge_types.task * string,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of string ] Eliom_parameters.param_name, [> `Registrable ])
  Eliom_services.service



val set_kind_service :
  (unit, Ocsforge_types.task * string option,
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   [ `One of string option ] Eliom_parameters.param_name, [> `Registrable ])
  Eliom_services.service



val new_task_service :
  (unit,
   Ocsforge_types.task *
   (string *
    (string *
     (CalendarLib.Calendar.Period.t option *
      (int32 option *
       (int32 option *
        (CalendarLib.Printer.Date.t option *
         (string * (string option * bool)))))))),
   [> `Nonattached of [> `Post ] Eliom_services.na_s ], [ `WithoutSuffix ],
   unit,
   [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
   ([ `One of string ] Eliom_parameters.param_name *
    ([ `One of string ] Eliom_parameters.param_name *
     ([ `One of CalendarLib.Calendar.Period.t option ] Eliom_parameters.param_name *
      ([ `One of int32 option ] Eliom_parameters.param_name *
       ([ `One of int32 option ] Eliom_parameters.param_name *
        ([ `One of CalendarLib.Printer.Date.t option ] Eliom_parameters.param_name *
         ([ `One of string ] Eliom_parameters.param_name *
          ([ `One of string option ] Eliom_parameters.param_name *
           [ `One of bool ] Eliom_parameters.param_name)))))))),
   [> `Registrable ])
  Eliom_services.service

