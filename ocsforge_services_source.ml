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


let source_service path project = Eliom_predefmod.Any.register_new_service
    ~path:[path; project; "sources"; ""]
    ~get_params:
    (Eliom_parameters.suffix_prod
       (Eliom_parameters.all_suffix "file")
       (Eliom_parameters.opt (Eliom_parameters.string "version") **
          (Eliom_parameters.bool "browse" **
             (Eliom_parameters.bool "view" **
                (Eliom_parameters.bool "annot" **
	           (Eliom_parameters.opt (Eliom_parameters.user_type 
				            Vm.string_to_pair 
				            Vm.pair_to_string "diff1") **
		      Eliom_parameters.opt (Eliom_parameters.user_type 
					      Vm.string_to_pair 
					      Vm.pair_to_string "diff2")))))))
    (fun sp (file,(version,(browse,(view,(annot,(d1,d2)))))) () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = Ocsforge_types.task_of_string project in
      let (title,page_content) = match (file,(version,(browse,(view,(annot,(d1,d2)))))) with
        | ([],(None,(_,(_,_)))) 
	| ([""],(None,(_,(_,_)))) ->
            (Some("Ocsforge - Repository browser"),
             Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:None)
	| ([],(_,(false,(false,(_,_))))) 
	| ([""],(_,(false,(false,(_,_))))) ->
	    (Some("Ocsforge - Repository browser"),
             Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:None)
        | (l,(None,(_,(_,(_,(Some(diff1),Some(diff2))))))) ->
	    (Some("Ocsforge - File diff"),
             Ocsforge_widgets_source.draw_diff_view ~sp ~id ~target:l ~diff1 ~diff2)
        | (l,(_,(false,(false,(false,(_,_)))))) ->
            (Some("Ocsforge - Repository browser"),
             Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:(Some(l)))
        | (l,(_,(true,(false,(_,(None,None)))))) -> 
	    (Some("Ocsforge - File browser"),
             Ocsforge_widgets_source.draw_file_page ~sp ~id ~target:l ~version)
	| (l,(_,(false,(false,(true,(None,None)))))) ->
            (Some("Ocsforge - File annotate"),
             Ocsforge_widgets_source.draw_annotate ~sp ~id ~target:l ~version)
	| (l,(_,(false,(true,(false,(None,None)))))) ->
	      (Some("Ocsforge - File content"),
               Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:l ~version)
	| _ -> (* TODO : gestion erreur ?*)
            (None,Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:[] ~version:None)
      in
      page_content >>= fun pc ->
      Ocsforge_data.get_area_for_task sp id >>= fun r_infos ->
      let gen_box _ = 
            Lwt.return (None,pc,Wiki_widgets_interface.Page_displayable,title)
      in
      Ocsisite.wikibox_widget#display_container 
            ~sp ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
            ~page:((Ocsigen_lib.string_of_url_path ~encode:true file),file)
            ~gen_box:gen_box
      >>= fun (html, code) ->
      Eliom_duce.Xhtml.send ~sp ~code html)



let log_service path project = Eliom_predefmod.Any.register_new_service
    ~path: [path; project; "log"]
    ~get_params: (Eliom_parameters.opt (Eliom_parameters.user_type  
                                          Vm.string_to_range 
				          Vm.range_to_string "range"))
    (fun sp range () ->
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = Ocsforge_types.task_of_string project in
      let (start_rev,end_rev) = match range with
        | None -> (None,None)
        | Some(sr,er) -> (sr,er)
      in
      Ocsforge_widgets_source.draw_log_table ~sp ~id ~file:None ~start_rev ~end_rev >>= fun pc ->
        Ocsforge_data.get_area_for_task sp id >>= fun r_infos ->
          let gen_box menu_style = 
            Lwt.return (None,pc,Wiki_widgets_interface.Page_displayable,Some("Ocsforge - Repository history"))
          in
          Ocsisite.wikibox_widget#display_container 
            ~sp ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
            ~page:((Ocsigen_lib.string_of_url_path ~encode:true []),[])
            ~gen_box:gen_box
            >>= fun (html, code) ->
              Eliom_duce.Xhtml.send ~sp ~code html)
 


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

