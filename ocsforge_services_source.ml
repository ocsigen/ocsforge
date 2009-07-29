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

let source_service path = Eliom_predefmod.Any.register_new_service
    ~path:[path; "sources"; ""]
    ~get_params:
    (Eliom_parameters.suffix_prod
       (Eliom_parameters.all_suffix "file")
       (Eliom_parameters.user_type
          Sh.string_to_kind 
          Sh.kind_to_string
          "view" **
          (Eliom_parameters.opt (Eliom_parameters.string "version") **
             (Eliom_parameters.opt (Eliom_parameters.string "to")))))
    (fun sp (file,(view,(v1,v2))) () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = path in 
      let (title,page_content) = match (file,(view,(v1,v2))) with
        | (l,(`Browse,(version,_))) ->
            begin match l with
            | [] | [""] ->
                (Some("Ocsforge - Repository browser"),
                 Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:None)
            | _ ->
                (Some("Ocsforge - Repository browser"),
                 Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:(Some(l)))
            end
	| (l,(`Diff,(Some(diff1),Some(diff2)))) ->
	    (Some("Ocsforge - File diff"),
             Ocsforge_widgets_source.draw_diff_view ~sp ~id ~target:l ~diff1 ~diff2)
        | (l,(`Options,(version,log_start))) -> 
	    (Some("Ocsforge - File browser"),
             Ocsforge_widgets_source.draw_file_page ~sp ~id ~target:l ~version ~log_start)
	| (l,(`Annot,(version,_))) ->
            (Some("Ocsforge - File annotate"),
             Ocsforge_widgets_source.draw_annotate ~sp ~id ~target:l ~version)
	| (l,(`Cat,(version,_))) ->
	      (Some("Ocsforge - File content"),
               Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:l ~version)
	| _ -> 
            (None,Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:[] ~version:None)
      in
      page_content >>= fun pc ->
      Ocsforge_data.get_area_for_page sp id >>= fun r_infos ->
      let gen_box _ = 
            Lwt.return (None,pc,Wiki_widgets_interface.Page_displayable,title)
      in
      Ocsisite.wikibox_widget#display_container 
            ~sp ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
            ~page:((Ocsigen_lib.string_of_url_path ~encode:true file),file)
            ~gen_box:gen_box
      >>= fun (html, code) ->
      Eliom_duce.Xhtml.send ~sp ~code html)


let log_service path = Eliom_predefmod.Any.register_new_service
    ~path: [path; "log"]
    ~get_params: (Eliom_parameters.opt (Eliom_parameters.user_type  
                                          Vm.string_to_range 
				          Vm.range_to_string "range"))
    (fun sp range () ->
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = path in
      let (start_rev,end_rev) = match range with
        | None -> (None,None)
        | Some(sr,er) -> (sr,er)
      in
      Ocsforge_widgets_source.draw_log_table ~sp ~id ~file:None ~start_rev ~end_rev >>= fun pc ->
        Ocsforge_data.get_area_for_page sp id >>= fun r_infos ->
          let gen_box _ = 
            Lwt.return
              (None,
               pc,
               Wiki_widgets_interface.Page_displayable,
               Some("Ocsforge - Repository history"))
          in
          Ocsisite.wikibox_widget#display_container 
            ~sp ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
            ~page:((Ocsigen_lib.string_of_url_path ~encode:true []),[])
            ~gen_box:gen_box
            >>= fun (html, code) ->
              Eliom_duce.Xhtml.send ~sp ~code html)
 


let register_repository_services () = 
  Ocsforge_sql.get_projects_path_list () >>= fun l ->
    Lwt_util.iter_serial
      (fun page ->
         Lwt.return (
           Ocsforge_services_hashtable.add_service 
             page
             { Sh.sources_service = source_service page;
               Sh.log_service = log_service page; }
         )
      )
      l

