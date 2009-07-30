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
module Params = Eliom_parameters

type file_tree = {{ <file_tree> [Xhtmltypes_duce.tr*]  }}

let source_service path = Eliom_predefmod.Any.register_new_service
    ~path:[path; "sources"; ""]
    ~get_params:
    (Params.suffix_prod
       (Params.all_suffix "file")
       (Params.opt (
          Params.user_type
          Sh.string_to_kind 
          Sh.kind_to_string
          "view") **
          (Params.opt (Params.string "version") **
             (Params.opt (Params.string "to")))))
    (fun sp (file,(view,(v1,v2))) () -> 
      let () =  Ocsforge_wikiext_common.send_css_up "ocsforge_sources.css" sp in
      let id = path in 
      let (title,page_content) = match (file,(view,(v1,v2))) with
        | (l,(None,(version,_))) ->
            begin match l with
            | [] | [""] ->
                (Some("Ocsforge - Repository browser"),
                 Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:None)
            | _ ->
                (Some("Ocsforge - Repository browser"),
                 Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version ~dir:(Some(l)))
            end
	| (l,(Some(`Diff),(Some(diff1),Some(diff2)))) ->
	    (Some("Ocsforge - File diff"),
             Ocsforge_widgets_source.draw_diff_view ~sp ~id ~target:l ~diff1 ~diff2)
        | (l,(Some(`Options),(version,log_start))) -> 
	    (Some("Ocsforge - File browser"),
             Ocsforge_widgets_source.draw_file_page ~sp ~id ~target:l ~version ~log_start)
	| (l,(Some(`Annot),(version,_))) ->
            (Some("Ocsforge - File annotate"),
             Ocsforge_widgets_source.draw_annotate ~sp ~id ~target:l ~version)
	| (l,(Some(`Cat),(version,_))) ->
	      (Some("Ocsforge - File content"),
               Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:l ~version)
	| _ -> 
            (Some("Ocsforge - wrong URL"),Ocsforge_widgets_source.draw_wrong_url_page ~sp ~id)
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
    ~get_params: (Params.opt (Params.user_type  
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
 

let register_xml_tree_service () = 
  let _ = 
    Eliom_duce.Xml.register_new_post_coservice'
    ~name:"ocsforge_repository_tree"
    ~post_params: (
          Params.opt (Params.string "dir") ** 
          Params.opt (Params.string "version"))
    (fun sp () (dir,version) ->
      let (path,dir_path) = match dir with 
      | None -> ("",[])
      | Some(p) -> 
          let list = Neturl.split_path p in 
          (List.hd list,List.tl list)
      in
      match Ocsforge_services_hashtable.find_service path with
      | None -> failwith "Project services not found"
      | Some(ps) ->
          let tr_list = 
            match dir_path with
            | [] -> 
                Ocsforge_widgets_source.xml_table_content
                  ~sp 
                  ~id:path 
                  ~version 
                  ~dir:None
                  ~project_services:ps
            | l ->
                Ocsforge_widgets_source.xml_table_content
                  ~sp 
                  ~id:path 
                  ~version 
                  ~dir:(Some(l))
                  ~project_services:ps
          in
          tr_list >>= fun tr ->
          Lwt.return ({{ <file_tree> tr }} : {{ Ocamlduce.Load.anyxml }})
    ) in Lwt.return ()


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


  
