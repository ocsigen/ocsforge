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

(* service temporaire qui ne fait rien *)
let temp_service = Eliom_predefmod.Action.register_new_service 
    ~path:["tmp"]
    ~get_params:(Eliom_parameters.suffix
		   ((Eliom_parameters.user_type
		       Ocsforge_types.task_of_string
		       Ocsforge_types.string_of_task "id") ** 
		      Eliom_parameters.string "version"))
    (fun sp (project,path) () ->  Lwt.return ())

let temp_source_service = Eliom_duce.Xhtml.register_new_service
    ~path:["sources"]
    ~get_params:(Eliom_parameters.suffix
		   ((Eliom_parameters.user_type
		       Ocsforge_types.task_of_string
		       Ocsforge_types.string_of_task "id") ** 
		      Eliom_parameters.all_suffix "path"))
    (fun sp (id,path) () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let page_content = match path with
        | [file; version] ->
	    Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~file ~version:(Some(version)) 
        | [file] ->
	    Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~file ~version:None    
	| _ -> Lwt.return {{ [ <table> [<tr> [<td> ['Error']]]] }}
      in page_content >>= fun pc ->
	Ocsimore_page.html_page ~sp pc)

(* Service pour l'historique des versions d'un projet *)
let log_service = Eliom_duce.Xhtml.register_new_service 
    ~path:["log"]
    ~get_params:(Eliom_parameters.suffix
		   ((Eliom_parameters.user_type
		       Ocsforge_types.task_of_string
		       Ocsforge_types.string_of_task "id")))
    (fun sp id () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      Ocsforge_widgets_source.draw_log_table ~sp ~id ~void_service:temp_service 
	>>= fun pc ->
	Ocsimore_page.html_page ~sp pc)
      
(* Service principal pour le dépot d'un projet *)
let project_repository_service project = Eliom_duce.Xhtml.register_new_service 
    (* Path a modifier pour mettre le NOM du projet *)
    ~path:[project;"sources"]
    ~get_params:(Eliom_parameters.suffix
		   ((Eliom_parameters.user_type
		       Ocsforge_types.task_of_string
		       Ocsforge_types.string_of_task "id") ** 
		      Eliom_parameters.string "version"))
    (fun sp (id,version) () ->
      let v = 
	if (String.length version == 0) then
	  None
	else Some(version)
      in
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version:v 
	~src_service:temp_source_service
	~log_service:log_service >>= 
      fun content ->
	Ocsimore_page.html_page ~sp content)


let register_repository_services = 
  let rec register_list l = match l with
    | [] -> Lwt.return ()
    | (page,root_task)::t -> 
	begin match (page,root_task) with
	| (Some(path),Some(task)) -> 
	    Ocsforge_services_hashtable.add_service 
	      (Ocsforge_types.task_of_sql task) 
	      (project_repository_service path);
	    register_list t
	| _ -> register_list t
	end
  in
  Ocsforge_sql.get_projects_path_list () >>= fun l ->
    register_list l

