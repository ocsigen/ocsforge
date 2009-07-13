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

let ( ** ) = Eliom_parameters.prod
let ( >>= ) = Lwt.bind
module Sh = Ocsforge_services_hashtable
module Vm = Ocsforge_version_managers

(* service temporaire qui ne fait rien *)
let temp_service = Eliom_predefmod.Action.register_new_service
    ~path:["voidservice"]
    ~get_params:(Eliom_parameters.suffix_prod
                   (Eliom_parameters.string "page_kind")
		   (Eliom_parameters.string "version"))
    (fun sp (page_kind,options) () -> Lwt.return ())

let source_service path project = Eliom_duce.Xhtml.register_new_service
    ~path:[path; project; "sources"; ""]
    ~get_params:
    (Eliom_parameters.suffix_prod
       (Eliom_parameters.all_suffix(*_user Neturl.split_path (Ocsigen_extensions.string_of_url_path ~encode:false)*) "file")
       (Eliom_parameters.opt (Eliom_parameters.string "version") **
          (Eliom_parameters.bool "browse" **
	     ((Eliom_parameters.opt (Eliom_parameters.user_type 
				       Vm.string_to_pair 
				       Vm.pair_to_string "diff1") **
		 Eliom_parameters.opt (Eliom_parameters.user_type 
					 Vm.string_to_pair 
					 Vm.pair_to_string "diff2"))))))
    (fun sp (file,(version,(browse,(d1,d2)))) () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = Ocsforge_types.task_of_string project in
      let page_content = match (file,(version,(browse,(d1,d2)))) with
        | ([],(None,(_,_))) 
	| ([""],(None,(_,_))) ->
            Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version:None
	| ([],(v,(_,_))) 
	| ([""],(v,(_,_))) ->
	    Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version:v
	| (l,(None,(true,(None,None)))) -> 
	    Ocsforge_widgets_source.draw_file_page ~sp ~id ~target:l ~version
	| (l,(Some(v),(true,(None,None)))) -> 
	    Ocsforge_widgets_source.draw_file_page ~sp ~id ~target:l ~version
	| (l,(Some(v),(false,(None,None)))) ->
	    if (String.compare v "latest" == 0) then
	      Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:l ~version:None
	    else
	      Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:l ~version:(Some(v))
	| (l,(None,(_,(Some(diff1),Some(diff2))))) ->
	    Ocsforge_widgets_source.draw_diff_view ~sp ~id ~target:l ~diff1 ~diff2
	| _ -> (* TODO : gestion erreur ?*)
            Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:[] ~version:None
      in
      page_content >>= fun pc ->
   Ocsimore_page.html_page ~sp pc)

let log_service path project = Eliom_duce.Xhtml.register_new_service
    ~path: [path; project; "log"]
    ~get_params:Eliom_parameters.unit
    (fun sp () () ->
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let s_id = Ocsforge_types.task_of_string project in
      Ocsforge_widgets_source.draw_log_table ~sp ~id:s_id ~file:None >>= fun pc ->
	Ocsimore_page.html_page ~sp pc
    )


let register_repository_services = 
  let rec register_list l = match l with
    | [] -> Lwt.return ()
    | (page,root_task)::t -> 
	begin match (page,root_task) with
	| (Some(path),Some(task)) ->
	    let rt = Ocsforge_types.task_of_sql task in
	    let st = Ocsforge_types.string_of_task rt in
	    Ocsforge_services_hashtable.add_service 
	      rt
	      {Sh.sources_service = source_service path st;
	       Sh.log_service = log_service path st;
	     };
	    register_list t
	| _ -> register_list t
	end
  in
  Ocsforge_sql.get_projects_path_list () >>= fun l ->
    register_list l

