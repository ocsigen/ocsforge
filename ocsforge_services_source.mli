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

val source_service :
    string ->
    string ->
    (string list * 
       (string option * (bool * (bool *((string * int) option * (string * int) option))))
       , unit,
     [ `Attached of
       Eliom_services.get_attached_service_kind Eliom_services.a_s ],
     [ `WithSuffix ],
     ([`One of string list] Eliom_parameters.param_name *
	([ `One of string ] Eliom_parameters.param_name *
	   ([ `One of bool ] Eliom_parameters.param_name * 
              ([ `One of bool ] Eliom_parameters.param_name * 
	         ([ `One of (string * int) ] Eliom_parameters.param_name *
		    ([ `One of (string * int) ] Eliom_parameters.param_name)))))), unit,
     [ `Registrable ])
	Eliom_services.service

val log_service : 
    string ->
    string ->
      (unit, unit,
       [ `Attached of
       Eliom_services.get_attached_service_kind Eliom_services.a_s ],
       [ `WithoutSuffix ],
       unit,unit,[ `Registrable ])
	Eliom_services.service

val temp_service :
    ((string * string ), unit,
     [ `Attached of
       Eliom_services.get_attached_service_kind Eliom_services.a_s ],
     [ `WithSuffix ],
     [ `One of string ] Eliom_parameters.param_name *
       [ `One of string ] Eliom_parameters.param_name, unit,
     [ `Registrable ])
    Eliom_services.service

val register_repository_services : unit Lwt.t
