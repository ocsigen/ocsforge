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

(** @author Granarolo Jean-Henri *)

open Eliom_pervasives

let ( ** ) = Eliom_parameters.prod
module Sh = Ocsforge_services_hashtable
module Vm = Ocsforge_version_managers
module Params = Eliom_parameters

(*
type file_tree = {{ <file_tree> [Xhtmltypes_duce.tr*]  }}
*)

let source_service path =
  let service_path = ( (Neturl.split_path path) @ [ "sources" ; "" ] ) in
  Eliom_output.Any.register_service
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
    (fun (file,(view,(v1,v2))) () ->
      Ocsforge_widgets_source.add_sources_css_header ();
      let id = path in
      lwt (title,page_content) =
	match (file,(view,(v1,v2))) with
          | (l,(None,(version,_))) ->
            begin match l with
              | [] | [""] ->
		lwt r = Ocsforge_widgets_source.draw_repository_table
                  ~id
                  ~version
                  ~dir:None in
		Lwt.return (Some("Ocsforge - Repository browser"),r)
              | _ ->
		try_lwt
	          lwt r = Ocsforge_widgets_source.draw_repository_table
		    ~id
		    ~version
		    ~dir:(Some(l)) in
		  Lwt.return (Some("Ocsforge - Repository browser"), r)
	        with
                  | Vm.Wrong_node_kind ->
                    lwt r = Ocsforge_widgets_source.draw_source_code_view
                      ~id
                      ~target:l
                      ~version in
                    Lwt.return (Some("Ocsforge - File content"), r)
            end
	  | (l,(Some(`Diff),(Some(diff1),Some(diff2)))) ->
	    lwt r = Ocsforge_widgets_source.draw_diff_view
              ~id
              ~target:l
              ~diff1
              ~diff2 in
            Lwt.return (Some("Ocsforge - File diff"),r)
          | (_,(Some(`PatchDiff),(Some(diff1),Some(diff2)))) ->
            lwt r = Ocsforge_widgets_source.draw_patchdiff
              ~id
              ~diff1
              ~diff2 in
            Lwt.return (Some("Ocsforge - Commit diff"),r)
          | (l,(Some(`Options),(version,log_start))) ->
	    lwt r = Ocsforge_widgets_source.draw_file_page
              ~id
              ~target:l
              ~version
              ~log_start in
            Lwt.return (Some("Ocsforge - File browser"),r)
          | (l,(Some(`Annot),(version,_))) ->
            lwt r = Ocsforge_widgets_source.draw_annotate
              ~id
              ~target:l
              ~version in
            Lwt.return (Some("Ocsforge - File annotate"),r)
          | (l,(Some(`Cat),(version,_))) ->
	    lwt r = Ocsforge_widgets_source.draw_source_code_view
              ~id
              ~target:l
              ~version in
            Lwt.return (Some("Ocsforge - File content"),r)
          | _ ->
            lwt r = Ocsforge_widgets_source.draw_wrong_url_page ~id in
            Lwt.return (Some("Ocsforge - wrong URL"),r)
      in
      lwt r_infos = Ocsforge_data.get_area_for_page id in
      lwt bi = Wiki.default_bi
        ~wikibox:r_infos.Ocsforge_types.r_sources_container
        ~rights:(Wiki_models.get_rights Wiki_site.wikicreole_model) in
      let gen_box1 _ =
        Lwt.return (Some(None,page_content))
      in
      let bi = { bi with
                 Wiki_widgets_interface.bi_subbox = gen_box1;
                 (*Wiki_types.bi_page = fst bi.Wiki_types.bi_page, ?? *)
               }
      in
      let gen_box _ =
        lwt page_content = Wiki_site.wikibox_widget#display_interactive_wikibox
          ~bi
          r_infos.Ocsforge_types.r_sources_container
	in
        Lwt.return (None, page_content,
                    Wiki_widgets_interface.Page_displayable,title)
      in
      lwt (html, code) =
	Wiki_site.wikibox_widget#display_container
          ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
          ~page:((Url.string_of_url_path ~encode:true file),file)
          ~gen_box:gen_box
      in
      Ocsimore_appl.send ~code html)

let log_service path =
  let service_path = ( Neturl.split_path path ) @ [ "log" ; "" ] in
  Eliom_output.Any.register_service
    ~path:service_path
    ~get_params: (Params.opt (Params.user_type
                                Vm.string_to_range
				Vm.range_to_string "range"))
    (fun range () ->
      Ocsforge_widgets_source.add_sources_css_header ();
      let id = path in
      let (start_rev,end_rev) = match range with
        | None -> (None,None)
        | Some(sr,er) -> (sr,er)
      in
      lwt page_content = Ocsforge_widgets_source.draw_log_page
        ~id
        ~file:None
        ~start_rev
        ~end_rev in
      lwt r_infos = Ocsforge_data.get_area_for_page id in
      lwt bi = Wiki.default_bi
        ~wikibox:r_infos.Ocsforge_types.r_sources_container
        ~rights:(Wiki_models.get_rights Wiki_site.wikicreole_model) in
      let gen_box1 _ =
        Lwt.return (Some(None,page_content))
      in
      let bi = { bi with
                    Wiki_widgets_interface.bi_subbox = gen_box1;
                    (*Wiki_types.bi_page = fst bi.Wiki_types.bi_page, ?? *) }
      in
      let gen_box _ =
        lwt page_content =
	  Wiki_site.wikibox_widget#display_interactive_wikibox
            ~bi
            r_infos.Ocsforge_types.r_sources_container in
        Lwt.return (None, page_content,
                    Wiki_widgets_interface.Page_displayable,
                    Some("Ocsforge - Repository history"))
      in
      lwt (html, code) = Wiki_site.wikibox_widget#display_container
        ~wiki:(r_infos.Ocsforge_types.r_wiki) ~menu_style:`Linear
        ~page:((Url.string_of_url_path ~encode:true []),[])
        ~gen_box:gen_box
      in
      Ocsimore_appl.send ~code html)

module SourceXml =
struct
  module XML = XML
  module Info =
  struct
    let content_type = "text/xml"
    let version = "ocsforge"
    let standard = Uri.uri_of_string "http://www.ocsigen.org/ocsforge/"
    let doctype =  XML_print.compose_doctype "xml" []
    let emptytags = [ ]
  end

  type 'a elt = XML.elt
  type doc = XML.elt
  let toelt x = x
  let doc_toelt x = x
end

module SourceXmlOutput =
  Eliom_output.Make_TypedXML_Registration(XML)(SourceXml)(struct
    type content = SourceXml.doc
  end)

let register_xml_tree_service () =
  let _ =
    SourceXmlOutput.register_post_coservice'
      ~name:"ocsforge_repository_tree"
      ~post_params: (
        Params.opt (Params.string "dir") **
        Params.opt (Params.string "version"))
      (fun () (dir,version) ->
	let (path,dir_path) = match dir with
	  | None -> ("",[])
	  | Some(p) ->
            let list = Neturl.split_path p in
            (List.hd list,List.tl list)
	in
	match Ocsforge_services_hashtable.find_service path with
	  | None -> failwith "Project services not found"
	  | Some(ps) ->
	    lwt tr =
              match dir_path with
		| [] ->
                  Ocsforge_widgets_source.xml_table_content
                    ~id:path
                    ~version
                    ~dir:None
                    ~project_services:ps
		| l ->
                  Ocsforge_widgets_source.xml_table_content
                    ~id:path
                    ~version
                    ~dir:(Some(l))
                    ~project_services:ps
            in
            Lwt.return [XML.node "file_tree" (HTML5.M.toeltl tr)]
      ) in Lwt.return ()

let register_repository_service page =
  Printf.printf "register repository service: %s\n%!" page;
  Lwt.return (
    Ocsforge_services_hashtable.add_service
      page
      { Sh.sources_service = source_service page ;
        Sh.log_service = log_service page ; }
  )

let register_repository_services () =
  lwt l = Ocsforge_sql.get_projects_path_list () in
  Lwt_util.iter_serial register_repository_service l
