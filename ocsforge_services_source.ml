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

let source_service path = 
  let service_path = ((Neturl.split_path path)@["sources"; ""]) in
  Eliom_predefmod.Any.register_new_service
    ~path:service_path
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
      (match (file,(view,(v1,v2))) with
        | (l,(None,(version,_))) ->
            begin match l with
            | [] | [""] ->
                Ocsforge_widgets_source.draw_repository_table 
                  ~sp 
                  ~id 
                  ~version 
                  ~dir:None >>= fun r ->
                  Lwt.return (Some("Ocsforge - Repository browser"),r)
            | _ ->
                Lwt.catch
                  (fun () ->
                    Ocsforge_widgets_source.draw_repository_table 
                      ~sp 
                      ~id 
                      ~version 
                      ~dir:(Some(l)) >>= fun r ->
                      Lwt.return (Some("Ocsforge - Repository browser"), r))
                  (function 
                    | Vm.Wrong_node_kind -> 
                        Ocsforge_widgets_source.draw_source_code_view 
                          ~sp 
                          ~id 
                          ~target:l 
                          ~version >>= fun r ->
                          Lwt.return (Some("Ocsforge - File content"), r)
                    | e -> Lwt.fail e)
            end
	| (l,(Some(`Diff),(Some(diff1),Some(diff2)))) ->
	    Ocsforge_widgets_source.draw_diff_view 
              ~sp 
              ~id 
              ~target:l 
              ~diff1 
              ~diff2 >>= fun r ->
              Lwt.return (Some("Ocsforge - File diff"),r)
        | (l,(Some(`Options),(version,log_start))) -> 
	    Ocsforge_widgets_source.draw_file_page 
              ~sp 
              ~id 
              ~target:l 
              ~version 
              ~log_start >>= fun r ->
              Lwt.return (Some("Ocsforge - File browser"),r)
        | (l,(Some(`Annot),(version,_))) ->
            Ocsforge_widgets_source.draw_annotate 
              ~sp 
              ~id 
              ~target:l 
              ~version >>= fun r ->
              Lwt.return (Some("Ocsforge - File annotate"),r)
        | (l,(Some(`Cat),(version,_))) ->
	    Ocsforge_widgets_source.draw_source_code_view 
              ~sp 
              ~id 
              ~target:l 
              ~version >>= fun r ->
            Lwt.return (Some("Ocsforge - File content"),r)
        | _ -> 
            Ocsforge_widgets_source.draw_wrong_url_page ~sp ~id >>= fun r ->
            Lwt.return (Some("Ocsforge - wrong URL"),r)
      ) >>= fun (title,page_content) ->
      Ocsforge_data.get_area_for_page sp id >>= fun r_infos ->
      Wiki.default_bi 
          ~sp 
          ~wikibox:r_infos.Ocsforge_types.r_sources_container 
          ~rights:(Wiki_models.get_rights Ocsisite.wikicreole_model) 
          >>= fun bi ->
      let gen_box1 _ =
        Lwt.return (Some(None,page_content))
      in
      let bi = { bi with 
                 Wiki_widgets_interface.bi_subbox = gen_box1;
                 (*Wiki_types.bi_page = fst bi.Wiki_types.bi_page, ?? *)
               }
      in      
      let gen_box _ = 
        Ocsisite.wikibox_widget#display_interactive_wikibox 
          ~bi 
          r_infos.Ocsforge_types.r_sources_container
          >>= fun page_content ->
          Lwt.return (None,{{ [page_content] }},Wiki_widgets_interface.Page_displayable,title)
      in
      Ocsisite.wikibox_widget#display_container 
            ~sp ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
            ~page:((Ocsigen_lib.string_of_url_path ~encode:true file),file)
            ~gen_box:gen_box
      >>= fun (html, code) ->
      Eliom_duce.Xhtml.send ~sp ~code html)


let log_service path = 
  let service_path = (Neturl.split_path path)@["log";""] in
  Eliom_predefmod.Any.register_new_service
    ~path:service_path
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
      Ocsforge_widgets_source.draw_log_page 
        ~sp 
        ~id 
        ~file:None 
        ~start_rev 
        ~end_rev >>= fun page_content ->
        Ocsforge_data.get_area_for_page sp id >>= fun r_infos ->
          Wiki.default_bi 
          ~sp 
          ~wikibox:r_infos.Ocsforge_types.r_sources_container 
          ~rights:(Wiki_models.get_rights Ocsisite.wikicreole_model) 
          >>= fun bi ->
          let gen_box1 _ =
            Lwt.return (Some(None,page_content))
          in
          let bi = { bi with 
                     Wiki_widgets_interface.bi_subbox = gen_box1;
                     (*Wiki_types.bi_page = fst bi.Wiki_types.bi_page, ?? *)
                   }
          in      
          let gen_box _ = 
            Ocsisite.wikibox_widget#display_interactive_wikibox 
              ~bi 
              r_infos.Ocsforge_types.r_sources_container
              >>= fun page_content ->
                Lwt.return (None,{{ [page_content] }},
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


  
