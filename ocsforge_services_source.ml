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


let project_repository_service = Eliom_duce.Xhtml.register_new_service 
    ~path:["sources"]
    ~get_params:(Eliom_parameters.suffix 
		   ((Eliom_parameters.user_type
		       Ocsforge_types.task_of_string
		       Ocsforge_types.string_of_task "id") ** 
		      Eliom_parameters.string "version"))
    (fun sp (id,version) () ->
      Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version >>= fun content ->
	Ocsimore_page.html_page ~sp content)
