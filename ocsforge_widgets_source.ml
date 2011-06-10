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

let (>>=) = Lwt.bind
module Data = Ocsforge_data
module Types = Ocsforge_types
module STypes = Ocsforge_source_types
module Vm = Ocsforge_version_managers
module Sh = Ocsforge_services_hashtable
open Eliom_pervasives

let _PAGE_SIZE = 100

let sources_css_header =
  Page_site.Header.create_header
    (fun () -> [Eliom_output.Html5.css_link
		(Page_site.static_file_uri ["ocsforge_sources.css"]) ()])


let add_sources_css_header () =
  Page_site.Header.require_header sources_css_header


let generate_css_style id css_class =
  if (!id mod 2 == 0) then
    (css_class^"_odd")
  else (css_class^"_even")

open HTML5.M

let generate_menu version page_kind
    (content : HTML5_types.tr HTML5.M.elt list )
    services : HTML5_types.table HTML5.M.elt =
  table ~a:[a_class ["sources_menu"]]
    (tr [
      td [
	table ~a:[a_class ["sources_submenu"]]
	  (
	    tr [
	      th ~a:[a_class ["submenu_title"]]
		[ pcdata "Repository" ]
	    ]
	  )
	  [
	    tr [
	      match (page_kind,version) with
		| (None,None) ->
		  td ~a:[a_class ["sources_menu_current"]]
		    [pcdata "Latest repository version "]
		| _ ->
		  td ~a:[a_class ["sources_menu_item"]]
		    [Eliom_output.Html5.a
			~a:[ a_title "Browse the repository";
			     a_class ["sources_menu_link"]]
			~service:services.Sh.sources_service
			[pcdata "Latest repository version"]
			([],(None,(None,None)))]
	    ];

	    tr [
	      match page_kind with
		| Some(`Log) ->
		  td ~a:[a_class ["sources_menu_current"]]
		    [ pcdata "Repository history" ]
		| _ ->
		  td ~a:[a_class ["sources_menu_item"]]
		    [
		      Eliom_output.Html5.a
			~a:[ a_title "View log";
			     a_class ["sources_menu_link"]]
			~service:services.Sh.log_service
			[pcdata "Repository history" ] None]
	    ]
	  ]
      ]
    ])
    content

let sources_page_content
    version
    ~kind
    (title_content : HTML5_types.flow5 HTML5.M.elt)
    menu_content
    main_content
    services : HTML5_types.flow5 HTML5.M.elt list =
  add_sources_css_header ();
  [ title_content;
    div ~a:[a_class ["sources_main_div"]]
      [
	div ~a:[a_class ["sources_menu_div"]]
	  [ generate_menu version kind menu_content services ];
	div ~a:[a_class ["sources_content_div"]] main_content;
      ]
  ]

let error (message:string) : HTML5_types.flow5 HTML5.M.elt Lwt.t =
  Lwt.return (
    div ~a:[a_class ["error_message"]] [
      span ~a:[ a_class ["message_title"]] [
	img ~alt:"Error"
	  ~src:(Page_site.static_file_uri
                  ~path:["message_error.png"]) ()];
      p [pcdata message]])


let warning (message:string) : HTML5_types.flow5 HTML5.M.elt Lwt.t =
  Lwt.return (
    div ~a:[ a_class ["warning_message"]] [
      span ~a:[ a_class ["message_title"]] [
	img ~alt:"Error"
	  ~src:(Page_site.static_file_uri
		  ~path:["message_warning.png"]) ()];
      p [pcdata message]])

let cut_string s size =
  if String.length s > size then
    ((String.sub s 0 (size-3))^"...")
  else s

let cut_author_mail aut =
  let l = Netstring_pcre.split (Netstring_pcre.regexp "@") aut in
  if (List.length l > 1) then
    ((List.hd l)^"@...")
  else
    aut

let utf8_span (spanclass:(string option)) s : [> HTML5_types.span ] HTML5.M.elt =
  match spanclass with
    | None -> span [pcdata s]
    | Some(sc) -> span ~a:[ a_class [sc] ] [pcdata s]

let rec utf8_lines l : [< HTML5_types.phrasing ] HTML5.M.elt list = match l with
  | [] -> []
  | h::t ->
    let b = utf8_lines t in
    (utf8_span None h)::(br ())::b

let utf8_td content (tdclass:string) : [> HTML5_types.td ] HTML5.M.elt Lwt.t =
  try_lwt
    let s = Netstring_pcre.split (Netstring_pcre.regexp "\n") content in
    Lwt.return
      (td ~a:[ a_class [ tdclass ]] (utf8_lines s))
  with
    | _ -> Lwt.return ( td ~a:[ a_class [tdclass]] [pcdata content] )

let build_path path = match path with
   | [] -> ("",[])
   | [f] -> (f,[])
   | _ -> let l = List.rev path in (List.hd l,List.tl l)

let rec path_title ~path ~version ~title ~ps = match path with
    | [] ->
      [span [pcdata title];
       span ~a:[ a_class ["path_element"]]
         [ Eliom_output.Html5.a
                  ~a:[a_class ["path"]]
                  ~service:ps.Sh.sources_service
                  [pcdata "root"]
                  ([],(None,(version,None)))];
       span ~a:[a_class ["path_delimiter"]] [pcdata "/"]]
    | h::t ->
        let b = path_title ~path:t ~version ~title ~ps in
        (b@
	   [span ~a:[ a_class ["path_element"]]
               [ Eliom_output.Html5.a
		   ~a:[a_class ["path"]]
		   ~service:ps.Sh.sources_service
		   [pcdata h]
		   ((List.rev path),(None,(version,None)))
	       ];
	    span ~a:[ a_class ["path_delimiter"]] [pcdata "/"]])

let repository_table_header =
  tr [ th ~a:[ a_class ["sources_table"]] [pcdata "File"];
       th ~a:[ a_class ["sources_table"]] [pcdata "Author"];
       th ~a:[ a_class ["sources_table"]] [pcdata "Latest version"] ]

let log_table_header =
  tr [ th ~a:[ a_class ["sources_table"]] [pcdata "Version"];
       th ~a:[ a_class ["sources_table"]] [pcdata "Author"];
       th ~a:[ a_class ["sources_table"]] [pcdata "Date"];
       th ~a:[ a_class ["sources_table"]] [pcdata "Comment"] ]


(*
let rec string_soption_list_of_list l =
  List.map
    (fun h ->
      (Eliom_output.Html5.Option ({{ {} }},
                                fst h,
			        Some(snd h),
                                false))) l
*)

let rec build_content dir_l ps cpt version tree depth = match tree with
  | STypes.File(f,aut,(rev_name,rev_id)) ->
    let file_path = match dir_l with
      | None -> [f]
      | Some(list_path) -> List.rev ((f)::(List.rev list_path))
    in
    Lwt.return
      [ tr ~a:[ a_class [
	if (!cpt mod 2 == 0) then begin
	  cpt := !cpt + 1;
	  "odd"
	end
	else begin
	  cpt := !cpt + 1;
	  "even"
	end
	]]
	  [ td ~a:[ a_class ["sources_table"]]
	      [ Eliom_output.Html5.a
		  ~a:[a_title "File history"]
		  ~service:ps.Sh.sources_service
		  [img ~alt:"file"
                      ~src:(Page_site.static_file_uri
                              ~path:["file_history.png"]) ()]
                  (file_path,(Some(`Options),(version,None)));
                Eliom_output.Html5.a
                  ~a:[a_title "View content"]
		  ~service:ps.Sh.sources_service
		  [pcdata f]
		  (file_path,(Some(`Cat),(version,None)));
	      ];

	    td ~a:[ a_class ["small_font_center"]] [pcdata aut];
	    td ~a:[ a_class ["small_ifont_center"]]
	      [ Eliom_output.Html5.a
                  ~a:[a_title "Browse the repository"]
		  ~service:ps.Sh.sources_service
		  [pcdata (cut_string rev_name 200)]
		  ([],(None,(Some(rev_id),None))) ]
          ]
      ]
  | STypes.Dir (d, l) ->
    let rec aux list dir res =
      match list with
	| []   -> Lwt.return res
	| h::t ->
          if (depth == 0) then
	    lwt built = build_content dir_l ps cpt version h (depth+1) in
	    (aux t dir ( built @ res ))
          else Lwt.return res
    in
    let dir_path = match dir_l with
      | None -> [d]
      | Some(list_path) -> List.rev ((d)::(List.rev list_path))
    in
    let a =
      if (String.length (d) > 0) && (depth>0) then
	[tr ~a:[ a_class ["folder"]]
	    [ td [img ~alt:"folder"
		     ~src:(Page_site.static_file_uri
                             ~path:["source_folder.png"]) ();
		  Eliom_output.Html5.a
		    ~a:[a_title "Browse directory"]
		    ~service:ps.Sh.sources_service
		    [pcdata (" "^(d))]
		    (dir_path,(None,(version,None)))];
	      td [];
	      td [];
	    ]
	]
      else []
    in
    let dir_name = d in
    aux l dir_name a

let xml_table_content ~id ~version ~dir ~project_services =
  let ps = project_services in
  lwt r_infos = Data.get_area_for_page id in
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
      begin
	try_lwt
	  lwt fun_pack = Ocsforge_version_managers.get_fun_pack kind in
	  let cpt = ref 0 in
	  let ls_call = match (version,dir) with
	    | (None,None) ->
	      fun_pack.STypes.vm_list path
	    | (Some(v),None) ->
	      fun_pack.STypes.vm_list ~id:v path
	    | (None,Some(l)) ->
	      fun_pack.STypes.vm_list ~dir:(String.concat "/" l) path
	    | (Some(v),Some(l)) ->
	      fun_pack.STypes.vm_list ~id:v ~dir:(String.concat "/" l) path
	  in
	  begin
	    try_lwt
	      lwt tree = ls_call in
	      build_content dir ps cpt version tree 0
	    with
	      | Vm.Node_not_found
	      | Vm.Revision_not_found -> Lwt.return []
	  end
	with
	  | Vm.Manager_not_supported -> Lwt.return []
      end
    | _ -> failwith "Unable to retrieve repository informations"


let create_repository_table_content ~id ~version ~dir ~project_services=
  let ps = project_services in
  lwt r_infos = Data.get_area_for_page id in
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
      Lwt.try_bind
	(fun () -> Ocsforge_version_managers.get_fun_pack kind)
	(fun fun_pack ->
	  let cpt = ref 0 in
	  let ls_call = match (version,dir) with
	    | (None,None) ->
	      fun_pack.STypes.vm_list path
	    | (Some(v),None) ->
	      fun_pack.STypes.vm_list ~id:v path
	    | (None,Some(l)) ->
	      fun_pack.STypes.vm_list ~dir:(String.concat "/" l) path
	    | (Some(v),Some(l)) ->
	      fun_pack.STypes.vm_list ~id:v ~dir:(String.concat "/" l) path
	  in
	  Lwt.try_bind
	    (fun () -> ls_call)
	    (fun tree ->
	      lwt b = build_content dir ps cpt version tree 0 in
	      Lwt.return
		(table ~a:[ a_class ["sources_table"]]
		    (repository_table_header)
		    ((match dir with
		      | None -> []
		      | Some(d) ->
			[tr ~a:[ a_class ["folder"]] [
			  td
			    [Eliom_output.Html5.a
				~service:ps.Sh.sources_service
				[img ~alt:"parent_directory"
				    ~src:(Page_site.static_file_uri
					    ~path:["parent_directory.png"]) ();
				 pcdata " ../"]
				(List.rev
				   (List.tl
				      (List.rev d)),
				 (None,(version,None)))];
			  td [];
			  td []]])@
			b)))
	    (function
	      | Vm.Node_not_found -> error "File or directory not found"
	      | Vm.Revision_not_found -> error "Revision not found"
	      | Vm.Wrong_node_kind ->
		begin match dir with
		  | None -> error "This is not a directory."
		  | Some(_) -> Lwt.fail Vm.Wrong_node_kind
		end
	      | Vm.Manager_command_error ->
		error "Version manager internal error"
	      | e -> Lwt.fail e))
	(function
	  | _ -> error "Manager not supported")
    | _ -> failwith "Unable to retrieve repository informations"


(* TODO: ocsforge_color

let create_source_code_content ~id ~file ~version =
  lwt r_infos = Data.get_area_for_page id in
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
      Lwt.try_bind
	(fun () -> Ocsforge_version_managers.get_fun_pack kind)
	(fun fun_pack ->
	  let cat_call =
	    begin match version with
	      | None -> fun_pack.STypes.vm_cat path file
	      | Some(v) ->
		fun_pack.STypes.vm_cat ~id:v path file
	    end
	  in
	  Lwt.try_bind
	    (fun () -> cat_call)
	    (fun s ->
	      if (String.length s = 0) then
		warning sp "Empty result"
	      else
		let formatted =
		  if s.[String.length s - 1] != '\n' then (s^"\n")
		  else s
		in
		Ocsforge_color.color_by_ext
		  (Lexing.from_string formatted) file >>= fun (lines,content) ->
		Lwt.return
		  (div ~a:[ a_class ["source_code_container"]] [
		    div ~a:[ a_class ["left_lines"]]
		      [pre ~a:[ a_class ["left_lines"]] lines];
		    div ~a:[ a_class ["source_code"]]
		      [pre ~a:[ a_class ["color"]] content];
		    div ~a:[ a_class ["right_lines"]]
		      [pre ~a:[ a_class ["right_lines"]] lines]
		  ]))
	    (function
	      | Vm.Node_not_found -> error sp "File or directory not found"
	      | Vm.Revision_not_found -> error sp "Revision not found"
	      | _ -> error sp "Version manager internal error"))
	(function _ -> error sp "Manager not supported")
    | (_,_) -> failwith "Unable to retrieve repository informations"

*)

(*





  let rec list_after l n = match l with
  | [] -> []
  | _::t ->
  if (n = 0) then l
  else list_after t (n-1)


let rec list_before l n = match l with
 | [] -> []
 | h::t ->
     if (n = 0) then [h]
     else h::(list_before t (n-1))


let rec extract_version_assoc_list log =
  List.map
    (fun p ->
      let name = cut_string !(p.STypes.name) 30 in
      (!(p.STypes.id),name))
    log


let find_name list id = List.assoc id list


let get_pos id l =
  let rec aux cpt l = match l with
  | [] -> -1
  | h::t ->
      if ((fst h) = id) then cpt
      else aux (cpt+1) t
  in aux 0 l


let split_log_list ~log_select ~start_rev ~end_rev =
  let last_elt = (List.length log_select - 1) in
  let (log_from,log_to) = match (start_rev,end_rev) with
  | (None,None) -> (0,(min last_elt (_PAGE_SIZE-1)))
  | (None,Some(e)) -> (0,(get_pos e log_select))
  | (Some(s),None) -> ((get_pos s log_select),(min last_elt (_PAGE_SIZE-1)))
  | (Some(s),Some(e)) -> ((get_pos s log_select),(get_pos e log_select))
  in
  let range = (fst (List.nth log_select log_to),
               fst (List.nth log_select log_from))
  in
  begin match (log_from,log_to) with
  | (0,e) ->
      if (e = last_elt) then (("",""),range,("",""))
      else
        let last_pos = min last_elt  (e + _PAGE_SIZE) in
        (("",""),
         range,
         (fst (List.nth log_select (e+1)),fst (List.nth log_select last_pos)))
  | (s,e) ->
      let first_pos = max 0 (s - _PAGE_SIZE) in
      let last_pos = min last_elt (e + _PAGE_SIZE) in
      if (e = last_elt) then
        ((fst (List.nth log_select first_pos),fst (List.nth log_select (s-1))),
         range,
         ("",""))
      else
        ((fst (List.nth log_select first_pos),fst (List.nth log_select (s-1))),
         range,
         (fst (List.nth log_select (e+1)),fst (List.nth log_select last_pos)))
  end



let create_log_links ~sp ~log_service ~log_select ~start_rev ~end_rev =
  let (a,b,c) = split_log_list ~log_select ~start_rev ~end_rev in
  let end_name = find_name log_select (snd b) in
  let start_name = find_name log_select (fst b) in
  (utf8_td ("Entries range: "^end_name^"  -  "^start_name) "middle") >>= fun middle ->
  Lwt.return ({{
               table ~a:[ a_class ["log_links"]][
                 <tr> [
                  {{
                   match a with
                   | ("",_) ->
                      {{ td ~a:[ a_class ["no_previous_entries"]]
                        [pcdata "(no previous entries)"] }}
                   | _ ->
                       {{ td ~a:[ a_class ["previous_entries_link"]]
                         [{:Eliom_output.Html5.a
                         ~a:[a_class ["log_link"]
		         ~service:log_service
		         ~sp
                         {{ [pcdata "< previous"] }}
                         (Some(Some(fst a),Some(snd a))) :}] }}
                 }}
                 !middle
                 {{
                   match c with
                   | ("",_) ->
                       {{ td ~a:[ a_class ["no_next_entries"]] [pcdata "(no next entries)"] }}
                   | _ ->
                       {{ td ~a:[ a_class ["next_entries_link"]]
                         [{:Eliom_output.Html5.a
                         ~a:[a_class ["log_link"]]
		         ~service:log_service
		         ~sp
                         {{ [pcdata "next >"] }}
                         (Some(Some(fst c),Some(snd c))) :}] }}
                 }}
               ]] }})



let log_table_content ~sp ~kind ~path ~log ~ps ~start_rev ~end_rev =
  let cpt = ref 0 in
  let rec extract_result log_result limit = match log_result with
    | [] -> Lwt.return {{ [] }}
    | p::t ->
        let next_content =
          if (limit = 1) then
            Lwt.return {{ [] }}
          else
            extract_result t (limit-1)
        in next_content >>= fun b ->
            (utf8_td (cut_string !(p.STypes.comment) 70) "small_ifont") >>=
              fun comment_td ->
	      Lwt.return
	          ({{ [tr ~a:[ a_class [{:
			          if (!cpt mod 2 == 0) then begin
			            cpt := !cpt + 1;
			            "odd"
			          end
			          else begin
			            cpt := !cpt + 1;
			            "even"
			          end
				      :}]]
	            [
                     td ~a:[ a_class ["small_font"]]
		        [
                          {:
                          if (kind = "file") then
                            {{ <span>
                               [{:
                                  Eliom_output.Html5.a
                                  ~a: {{ {title="View this version" } }}
                                  ~service: ps.Sh.sources_service
                                  ~sp
                                  {{ [img ~alt:"file content"
			               ~src:{:
                                            Page_site.static_file_uri
                                            ~sp
                                            ~path:["source_file.png"] :}>[]] }}
                                   (path,
                                    (Some(`Cat),
                                     (Some(!(p.STypes.id)),None))) :}] }}
                          else match t with
                           | [] -> {{ <span> [] }}
                           | previous::_ ->
                               {{ <span>[{:
                                  Eliom_output.Html5.a
                                  ~a: {{ {title="Diff to previous" } }}
                                  ~service: ps.Sh.sources_service
                                  ~sp
                                  {{ [img ~alt:"diff"
			                ~src:{:
                                        Page_site.static_file_uri
                                        ~sp
                                        ~path:["diff_to_previous.png"] :}>[]] }}
                                   ([],
                                    (Some(`PatchDiff),
                                     (Some(!(previous.STypes.id)),
                                      Some(!(p.STypes.id))))) :}] }}
                         :}
                         ' '
                         {: Eliom_output.Html5.a
                            ~a: {{ {title="Browse the repository"} }}
			    ~service: ps.Sh.sources_service
			    ~sp (pcdata cut_string !(p.STypes.name) 50)
			    ([],(None,(Some(!(p.STypes.id)),None)))
			    :}
                        ]
		      td ~a:[ a_class ["small_font"]]
                        {: (cut_author_mail !(p.STypes.author)) :}
	              td ~a:[ a_class ["xsmall_font"]]
                        {: !(p.STypes.date) :}
                          !comment_td] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  let vl = extract_version_assoc_list log in
  let log =
    match (start_rev,end_rev) with
    | (None,None) -> log
    | (None,Some(er)) ->
        let end_pos = get_pos er vl in
        list_before log end_pos
    | (Some(sr),None) ->
        let start_pos = get_pos sr vl in
        list_after log start_pos
    | (Some(sr),Some(er)) ->
        let end_pos = get_pos er vl in
        let tmp = list_before vl end_pos in
        let start_pos = get_pos sr tmp in
        list_after (list_before log end_pos) start_pos
  in extract_result log _PAGE_SIZE


let create_log_page_content ~sp ~id ~file ~range ~project_services =
  let ps = project_services in
  let (start_rev,end_rev) = match range with
  | None -> (None,None)
  | Some(r) -> (fst r ,snd r)
  in
  Data.get_area_for_page sp id >>= fun r_infos ->
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
  | (Some(kind),Some(path)) ->
      Lwt.try_bind
        (fun () -> Ocsforge_version_managers.get_fun_pack kind)
	(fun fun_pack ->
	  let log = match (file,range) with
	  | (None,None) ->
              fun_pack.STypes.vm_log ~limit:_PAGE_SIZE path
	  | (Some(f),None) ->
              fun_pack.STypes.vm_log ~file:f ~limit:_PAGE_SIZE path
          | (None,Some(r)) ->
              fun_pack.STypes.vm_log ~range:r ~limit:_PAGE_SIZE path
          | (Some(f),Some(r)) ->
              fun_pack.STypes.vm_log ~file:f ~range:r ~limit:_PAGE_SIZE path
	  in
          Lwt.try_bind
	  (fun () -> log)
          (fun log_result ->
            let assoc_list = extract_version_assoc_list log_result in
            let select_list = string_soption_list_of_list assoc_list in
            let commit_diff_form
                ((file,(kind,(diff1,diff2))) :
                   ([`One of string list] Eliom_parameters.param_name *
	              ([ `One of Sh.src_page_kind ] Eliom_parameters.param_name *
                         ([ `One of string ] Eliom_parameters.param_name *
                            ([ `One of string ] Eliom_parameters.param_name))))) =

              {{[p ~a:[ a_class ["menu_form"]] [
                span ~a:[ a_class ["menu_form_title"]] [pcdata "Commit diff"]
                    {:
		       Eliom_output.Html5.user_type_input
		       (Ocsigen_lib.string_of_url_path
                          ~encode:false)
		       ~input_type: {: "hidden" :}
		       ~name: file
		       ~value: []
		       ()
		       :}
                    <br>[]
	            'From'<br>[]
	            {:
	               Eliom_output.Html5.string_select
	               ~a:[a_class ["version_select"]]
	               ~name: diff1
	               (List.hd select_list)
                       (List.tl select_list)
                       :}
	            <br> []
	            'To' <br> []
	            {:
	               Eliom_output.Html5.string_select
	               ~a:[a_class ["version_select"]]
	               ~name: diff2
	               (List.hd select_list)
                       (List.tl select_list)
                       :}
                    <br>[]
	            {: Eliom_output.Html5.user_type_button
                       Sh.kind_to_string
                       ~name: kind
	               ~value: `PatchDiff
	               {{ "Execute diff" }}
	               :}
              ]] }}
            in
            log_table_content ~sp ~kind:"log" ~path:[] ~log:log_result ~ps ~start_rev ~end_rev >>=
            fun b ->
            create_log_links
                ~sp
                ~log_service:ps.Sh.log_service
                ~log_select:assoc_list
                ~start_rev
                ~end_rev >>= fun (linktable) ->
                  Lwt.return
                    ({{ [ <tr> [
                                 <td> [
                                  table ~a:[a_class ["sources_submenu"]] [
                                   <tr> [
                                    <th class="submenu_title">
                                    [ 'Log' ]
                                   ]
                                  <tr> [
                                    td ~a:[a_class ["sources_menu_item"]]
			              [ {: Eliom_output.Html5.get_form
                                           ~a:[a_class ["version_select"]]
				           ~service: ps.Sh.sources_service
				           ~sp
				           (commit_diff_form) :}
			              ]
                                  ]]]]] }},
                     {{ [
                        linktable
                          table ~a:[a_class ["log_table"]]
		            [!log_table_header !b]] }}))
            (fun exn ->
                let error_content = match exn with
                | Vm.Node_not_found -> error sp "File or directory not found"
                | Vm.Revision_not_found -> error sp "Revision not found"
                | _ -> error sp "Version manager internal error"
                in error_content >>= fun content ->
                  Lwt.return ({{ [] }},content)))
        (function _ ->
          error sp "Manager not supported" >>= fun e ->
            Lwt.return ({{ [] }},e))
    | _ -> failwith "Unable to retrieve repository informations"



let create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 =
  let rec extract_diff_result
      (span_class:string)
      (content_list:((STypes.rowType*string) list)) = match content_list with
      | [] -> Lwt.return {{ [] }}
      | (rt,s)::t ->
	  extract_diff_result span_class t >>= fun b ->
	    let span = begin match rt with
	    | STypes.Common ->
                utf8_span (Some("common")) (s^"\n")
	    | STypes.Diff ->
                utf8_span (Some(span_class)) (s^"\n")
	    | STypes.Blank ->
                utf8_span (Some("blank")) (s^"\n")
	    end
            in
            Lwt.return ({{ [ span !b ]  }} :
                          {{ [Xhtmltypes_duce.special_pre*] }})
  in
  Data.get_area_for_page sp id >>= fun r_infos ->
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
  | (Some(kind),Some(path)) ->
      Lwt.try_bind
        (fun () -> Ocsforge_version_managers.get_fun_pack kind)
	(fun fun_pack ->
	  let (old_file,new_file) = (diff1,diff2) in
	  Lwt.try_bind
            (fun () -> fun_pack.STypes.vm_diff file path old_file new_file)
            (fun diff_result ->
	      extract_diff_result
                "old"
                diff_result.STypes.oldContent >>= fun oldf ->
	      extract_diff_result
                "new"
                diff_result.STypes.newContent >>= fun newf ->
	      Lwt.return ({{ [
			     <div class="diff">[
			     <span class="diff_title"> {: ("@ "^old_file) :}
			     <pre class="diff"> {: oldf :}]
			     <div class="diff">[
			       <span class="diff_title"> {: ("@ "^new_file) :}
			       <pre class="diff"> {: newf :}
                             ]
			   ] }} : {{ [Xhtmltypes_duce.block*] }}))
            (function
              | Vm.Node_not_found -> error sp "File or directory not found"
              | Vm.Revision_not_found -> error sp "Revision not found"
              | Vm.Manager_command_error ->
                  error sp "Version manager internal error"
              | e -> Lwt.fail e))
        (function
          | Vm.Manager_not_supported -> error sp "Manager not supported"
          | e -> Lwt.fail e)
  | _ -> failwith "Unable to retrieve repository informations"


let create_patchdiff ~sp ~id ~diff1 ~diff2 =
  let rec extract_patchdiff l = match l with
    | [] -> Lwt.return {{ [] }}
    | h::t ->
        extract_patchdiff t >>= fun b ->
          let span =
            if (String.length h > 0) then
              if (h.[0] = '-') then
                utf8_span (Some("old")) (h^"\n")
              else if (h.[0] = '+') then
                utf8_span (Some("new")) (h^"\n")
              else
                utf8_span (Some("common")) (h^"\n")
            else
              utf8_span (Some("common")) (h^"\n")
          in
          Lwt.return ({{ [ span !b ]  }} :
                        {{ [Xhtmltypes_duce.special_pre*] }})
  in
  Data.get_area_for_page sp id >>= fun r_infos ->
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
  | (Some(kind),Some(path)) ->
      Lwt.try_bind
        (fun () -> Ocsforge_version_managers.get_fun_pack kind)
	(fun fun_pack ->
          Lwt.try_bind
            (fun () -> fun_pack.STypes.vm_patchdiff path diff1 diff2)
            (fun diff_content ->
                let l =
                  Netstring_pcre.split (Netstring_pcre.regexp "\n") diff_content
                in
                if (diff_content != ""
                   && List.length l > 0
                   && List.nth l 0 != "") then
                extract_patchdiff l >>= fun extracted_content ->
                  Lwt.return
                    ({{ [
                        div ~a:[a_class ["patchdiff"]] [
                          pre ~a:[a_class ["diff"]] extracted_content
                        ] ] }} : {{ [Xhtmltypes_duce.block* ] }})
              else
                Lwt.return ({{ [div ~a:[a_class ["patchdiff"]] [
                                 pre ~a:[a_class ["diff"]]
                                   {: "Empty diff output \n\t- Ocsforge -" :}
                               ]] }} : {{ [Xhtmltypes_duce.block* ] }})
            )
            (function
              | Vm.Revision_not_found ->
                  error sp "Revision not found"
              | Vm.Manager_command_error ->
                  error sp "Version manager internal error"
              | e -> Lwt.fail e))
        (function
          | Vm.Manager_not_supported -> error sp "Manager not supported"
          | e -> Lwt.fail e)
  | _ -> failwith "Unable to retrieve repository informations"




let create_file_log_links
    ~sp
    ~project_services
    ~target
    ~version
    ~log_start
    ~log_result =
  let ps = project_services in
  let range_start = List.hd log_result in
  let range_end =
    if (List.length log_result >= _PAGE_SIZE) then
      List.nth log_result (_PAGE_SIZE-1)
    else
      if (List.length log_result = 1) then range_start
      else List.hd (List.rev (List.tl log_result))
  in
  let range = ("Entries range: "^(snd (range_start)^
               "  -  "^
               (snd (range_end)))) in
  (utf8_td range "middle") >>= fun middle ->
   Lwt.return
      {{ table ~a:[a_class ["log_links"]] [
          <tr> [
          {{ match log_start with
          | None ->
              {{ td ~a:[a_class ["no_previous_entries"]]
                {: "(no previous entries)" :} }}
          | Some(_) ->
              {{ td ~a:[a_class ["previous_entries_link"]]
                [{: Eliom_output.Html5.a
		    ~a:[a_class ["log_link"]]
		    ~service:ps.Sh.sources_service
		    ~sp (pcdata "First page")
                    (target,(Some(`Options),(version,None))):}] }} }}
          !middle
          {{ if (List.length log_result > _PAGE_SIZE) then
            {{ td ~a:[a_class ["next_entries_link"]]
              [{: Eliom_output.Html5.a
		  ~a:[a_class ["log_link"]]
		  ~service:ps.Sh.sources_service
		  ~sp (pcdata "Next log entries")
                  (target,(Some(`Options),(version,Some((fst(range_end)))))) :}]
               }}
          else
            {{ td ~a:[a_class ["no_next_entries"]]
              {: "(no next entries)" :} }}
        }}
      ]] }}


let create_file_page ~sp ~id ~target ~version ~log_start ~project_services =
  let ps = project_services in
  Data.get_area_for_page sp id >>= fun r_infos ->
  match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
  | (Some(kind),Some(path)) ->
      Lwt.try_bind
      (fun () -> Ocsforge_version_managers.get_fun_pack kind)
      (fun fun_pack ->
	let file_path = String.concat "/" target in
        let log_call =
          fun_pack.STypes.vm_log
            ~file:file_path
            ~range:(None,log_start)
            ~limit:(_PAGE_SIZE) path
	in
        Lwt.try_bind
        (fun () -> log_call)
	(fun log_result ->
          log_table_content
            ~sp
            ~kind:"file"
            ~path:target
            ~log:log_result
            ~ps
            ~start_rev:log_start
            ~end_rev:None >>= fun log ->
	      let v_list = extract_version_assoc_list log_result in
	      let select_list = string_soption_list_of_list v_list in
	      let file_version_select_form
		  ((file,(kind,(version,_))) :
                  ([`One of string list] Eliom_parameters.param_name *
		  ([ `One of Sh.src_page_kind ] Eliom_parameters.param_name *
                  ([ `One of string ] Eliom_parameters.param_name *
                  ([ `One of string ] Eliom_parameters.param_name))))) =
		    {{ [ p ~a:[a_class ["menu_form"]] [
                          span ~a:[a_class ["menu_form_title"]]
                            [pcdata "Content viewing"]
                          <br> []
		          {:
		             Eliom_output.Html5.user_type_input
		             (Ocsigen_lib.string_of_url_path
                                ~encode:false)
		             ~input_type: {: "hidden" :}
		             ~name: file
		             ~value: target
		             ()
		             :}
                          'Select a version  '
		          !{: match select_list with
                          | [] -> {{ [] }}
                          | _ ->
                              {{ [{:
                                     Eliom_output.Html5.string_select
                                     ~a:[a_class ["version_select"]]
		                     ~name: version
		                     (List.hd select_list)
                                     (List.tl select_list)
                                     :}] }}
                                :}
                          {:
		             Eliom_output.Html5.user_type_button
                             Sh.kind_to_string
                             ~name: kind
		             ~value:`Cat
		             (pcdata "View code")
		             :}
                          {:
		             Eliom_output.Html5.user_type_button
                             Sh.kind_to_string
                             ~name: kind
		             ~value: `Annot
		             (pcdata "Annotate")
		             :}
		    ]] }}
	      in
	      let file_diff_form
		  ((file,(kind,(diff1,diff2))) :
                  ([`One of string list] Eliom_parameters.param_name *
		  ([ `One of Sh.src_page_kind ] Eliom_parameters.param_name *
                  ([ `One of string ] Eliom_parameters.param_name *
                  ([ `One of string ] Eliom_parameters.param_name))))) =
                    {{[p ~a:[a_class ["menu_form"]] [
                        span ~a:[a_class ["menu_form_title"]] [pcdata "Content diff"]
                        <br>[]
		        {:
		           Eliom_output.Html5.user_type_input
		           (Ocsigen_lib.string_of_url_path ~encode:false)
		           ~input_type: {: "hidden" :}
		           ~name: file
		           ~value: target
		           ()
		        :}
		        'From'<br>[]
		        {:
		           Eliom_output.Html5.string_select
		           ~a:[a_class ["version_select"]]
		           ~name: diff1
		           (List.hd select_list)
                           (List.tl select_list)
                        :}
		        <br> []
		        'To' <br> []
		        {:
		           Eliom_output.Html5.string_select
		           ~a:[a_class ["version_select"]]
		           ~name: diff2
		           (List.hd select_list)
                           (List.tl select_list)
                           :}
                        <br>[]
		        {: Eliom_output.Html5.user_type_button
                           Sh.kind_to_string
                           ~name: kind
		           ~value: `Diff
		           {{ "Execute diff" }}
		        :}
		    ]] }}
	      in
              Lwt.try_bind
              (fun () ->
                create_file_log_links
                  ~sp
                  ~project_services:ps
                  ~target
                  ~version
                  ~log_start
                  ~log_result:v_list)
              (fun log_links ->
                Lwt.return (({{ [
                                <tr> [
                                 <td> [
                                  table ~a:[a_class ["sources_submenu"]] [
                                   <tr> [
                                    th ~a:[a_class ["submenu_title"]]
                                    {: "File" :}
                                   ]
                                   <tr> [
                                     td ~a:[a_class ["sources_menu_item"]]
                                      [ {: Eliom_output.Html5.a
                                           ~a: {{
                                                {class="sources_menu_link"
                                                 title="Latest content"} }}
		                           ~service:ps.Sh.sources_service
		                           ~sp (pcdata "View content")
		                           (target,(Some(`Cat),(None,None)))
                                      :} ]]
                                   <tr> [
                                     td ~a:[a_class ["sources_menu_current"]]
                                      [pcdata "File history"]]
                                   <tr> [
                                     td ~a:[a_class ["sources_menu_item"]]
                                       [ {: Eliom_output.Html5.a
                                           ~a: {{
                                                {class="sources_menu_link"
                                                 title="Latest content"} }}
		                           ~service:ps.Sh.sources_service
		                           ~sp (pcdata "Annotate")
		                           (target,(Some(`Annot),(None,None)))
                                       :} ]]
                                   ]]]
                                   <tr> [
                                    <td> [
                                     table ~a:[a_class ["sources_submenu"]] [
                                      <tr> [
                                       th ~a:[a_class ["submenu_title"]]
                                         {: "History browsing" :}
                                      ]
                                      <tr> [
                                        td ~a:[a_class ["sources_menu_item"]]
			                  [ {: Eliom_output.Html5.get_form
                                           ~a:[a_class ["version_select"]]
				           ~service: ps.Sh.sources_service
				           ~sp
				           (file_version_select_form) :}
			                  ]
                                       ]
                                      !{:
                                        match select_list with
                                        | [] -> {{ [] }}
                                        | _ ->
                                         {{ [tr ~a:[a_class ["sources_menu"]] [
                                           td ~a:[a_class ["sources_menu_item"]]
				             [{:
                                              Eliom_output.Html5.get_form
                                              ~a:[a_class ["version_select"]]
				              ~service: ps.Sh.sources_service
				              ~sp
				               file_diff_form :}]
				         ]] }} :}
                                     ]]]] }}, {{
                                     [   log_links
                                         table ~a:[a_class ["log_table"]]
                                           [!log_table_header !log] ] }})
			      : (  {{ [ Xhtmltypes_duce.tr* ] }} *
                                     {{ Xhtmltypes_duce.flows }})))
                (fun _ ->
                  error sp "File not found" >>= fun c ->
                  Lwt.return ({{ [] }},c)))
	  (fun exn ->
            let error_content = match exn with
            | Vm.Node_not_found -> error sp "File or directory not found"
            | Vm.Revision_not_found -> error sp "Revision not found"
            | _ -> error sp "Version manager internal error"
            in error_content >>= fun content ->
            Lwt.return ({{ [] }},content)))
          (function _ ->
            error sp "Manager not supported" >>= fun c ->
            Lwt.return ({{ [] }},c))
    | _ -> failwith "Unable to retrieve repository informations"


let generate_color int =
  let i = int mod 0xffffff in
  let (r,g,b) = ((i land 0xff0000) lsr 16,(i land 0xff00) lsr 8,i land 0xff) in
  let rref = ref r in
  let gref = ref g in
  let bref = ref b in
  while (!rref + !gref + !bref < 450) do
    rref := (int_of_float ((float_of_int !rref) *. 1.2) + 1) mod 256;
    gref := (int_of_float ((float_of_int !gref) *. 1.3) + 1) mod 256;
    bref := (int_of_float ((float_of_int !bref) *. 1.1) + 1) mod 256;
  done;
  Printf.sprintf "rgb(%d,%d,%d)" !rref !gref !bref


let create_annotate_page ~sp ~id ~target ~version ~project_services:_ =
  let color_table:(int,string) Hashtbl.t = Hashtbl.create 10 in
  let rec extract_annotate_result (l:(string*string) list) = match l with
    | [] -> Lwt.return ({{ [] }},{{ [] }})
    | (aut,line)::t ->
        let aut_hash = (Hashtbl.hash aut) in
        let color =
          begin try
            Hashtbl.find color_table aut_hash
          with Not_found ->
            let c = generate_color aut_hash in
            Hashtbl.add color_table aut_hash c;
            c
          end
        in
        extract_annotate_result t >>= fun (a,b) ->
        Lwt.try_bind
        (fun () -> Lwt.return (Ocamlduce.Utf8.make line))
        (fun utf8 ->
          Lwt.return (({{ [<span style=
                            {:("display:block; background-color:"^color):}>
                            {: (cut_author_mail aut) :}  !a]}},
                           {{ [
                              span ~a:[a_class ["annot" style=
                                {:("display:block; background-color:"^color):}]]
                                {: utf8 :}
                           !b] }})
                        : ({{ [Xhtmltypes_duce.span*] }}*
                             {{ [Xhtmltypes_duce.span*] }})))
        (function _ ->
          Lwt.return (({{ [<span style=
                            {:("display:block; background-color:"^color):}>
                            {: (cut_author_mail aut) :}  !a]}},
                       {{ [
                          span ~a:[a_class ["annot" style=
                            {:("display:block; background-color:"^color):}]]
                            {: line :}
                        !b] }})
                        : ({{ [Xhtmltypes_duce.span*] }}*
                             {{ [Xhtmltypes_duce.span*] }})))
  in
  Data.get_area_for_page sp id >>= fun r_infos ->
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
        Lwt.try_bind
          (fun () -> Ocsforge_version_managers.get_fun_pack kind)
	  (fun fun_pack ->
            let file_path = String.concat "/" target in
            let annot_command = match version with
            | None -> fun_pack.STypes.vm_annot path file_path
            | Some(v) -> fun_pack.STypes.vm_annot ~id:v path file_path
            in
            Lwt.try_bind
              (fun () -> annot_command)
              (fun annot_result ->
                extract_annotate_result annot_result >>= fun (a,b) ->
                  Lwt.return {{ [
                                div ~a:[a_class ["annot_author"]] [
                                  pre ~a:[a_class ["annot"]] [!a]
                                ]
                                    div ~a:[a_class ["annot_content"]] [
                                      pre ~a:[a_class ["annot"]] [<code>[!b]]
                                    ]
                              ] }})
              (function
                | Vm.Node_not_found -> error sp "File or directory not found"
                | Vm.Revision_not_found -> error sp "Revision not found"
                | _ -> error sp "Version manager internal error"))
          (function _ -> error sp "Manager not supported")
    | _ -> failwith "Unable to retrieve repository informations"



let draw_repository_table ~sp ~id ~version ~dir =
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let (title,path,table) = match (version,dir) with
      | (None,None) ->
          (" – latest version",[],
           create_repository_table_content ~sp ~id ~version ~dir:None
             ~project_services:ps)
      | (None,Some(rep)) ->
          (" – latest version",rep,
           (create_repository_table_content
              ~sp ~id ~version
              ~dir ~project_services:ps))
      | (Some(v),None) -> ((" – version "^v),[],
                           create_repository_table_content ~sp ~id ~version
                             ~dir:None
                             ~project_services:ps)
      | (Some(v),Some(rep)) ->
          ((" – version "^v),rep,
           create_repository_table_content ~sp ~id ~version
             ~dir ~project_services:ps)

      in
      let (current_dir,current_dir_path) = build_path path in
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"" ~ps) in
      table >>= fun b ->
      let menu_content = {{ [] }} in
      let title_content =
        {{
         div ~a:[a_class ["path"]] [
           !a
           span ~a:[a_class ["last_path_element"]] {: current_dir :}
           {{ utf8_span None title }}
         ] }}
      in
      Lwt.return ({{
                      {{
                       (sources_page_content
                          sp
                          version
                          ~kind:None
                          title_content
                          menu_content
                          b
                          ps) }}
		     }}
		    : {{ Xhtmltypes_duce.flows }})


let draw_source_code_view ~sp ~id ~target ~version =
  let file = Ocsigen_lib.string_of_url_path ~encode:false target in
  let (current_dir,current_dir_path) = build_path target in
  let dir_version = match version with
    | None -> " – latest version"
    | Some(v) -> (" – version "^v)
  in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"" ~ps) in
      create_source_code_content ~sp ~id ~file ~version >>= fun b ->
      let menu_content =
        {{
         [<tr> [
           <td> [
            table ~a:[a_class ["sources_submenu"]] [
             <tr> [
              th ~a:[a_class ["submenu_title"]]
                {: "File" :}
             ]
             <tr> [
              td ~a:[a_class ["sources_menu_current"]] {: "View content" :}]
             <tr>[
              td ~a:[a_class ["sources_menu_item"]]
	        [{:
		  Eliom_output.Html5.a
		  ~service:ps.Sh.sources_service
                  ~a:[a_class ["sources_menu_link"]]
		  ~sp (pcdata "File history")}
		  (target,(Some(`Options),(None,None))) :}]
              ]
             <tr> [
              td ~a:[a_class ["sources_menu_item"]]
                [{:
                    Eliom_output.Html5.a
                    ~a:[a_class ["sources_menu_link"]]
		    ~service:ps.Sh.sources_service
		    ~sp (pcdata "Annotate")}
		   (target,(Some(`Annot),(version,None))) :}]
            ]
           ]
          ]
         ]]
       }}
      in
      let title_content =
         {{
          div ~a:[a_class ["path"]] [ !a
                                 span ~a:[a_class ["last_path_element"]] {: current_dir :}
                                 {{ utf8_span None dir_version }}] }}
      in
      Lwt.return ({{
                     {{ (sources_page_content
                               sp
                               version
                               ~kind:(Some(`Cat))
                               title_content menu_content b ps) }}
                         }} : {{ Xhtmltypes_duce.flows }})


let draw_log_page ~sp ~id ~file ~start_rev ~end_rev =
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      create_log_page_content
        ~sp
        ~id
        ~file
        ~range:(Some(start_rev,end_rev))
        ~project_services:ps >>=
      fun (menu_content,page_content) ->
        let title_content = {{ div ~a:[a_class ["path"]] [pcdata "Repository history"] }}
        in
        Lwt.return ({{
		       {{ (sources_page_content
                             sp
                             None
                             ~kind:(Some(`Log))
                             title_content menu_content page_content ps) }}
                     }} : {{ Xhtmltypes_duce.flows }})


let draw_diff_view ~sp ~id ~target ~diff1 ~diff2 =
  let file = Ocsigen_lib.string_of_url_path ~encode:false target in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a =
        path_title
          ~sp
          ~path:current_dir_path
          ~version:None
          ~title:"File diff - "
          ~ps
      in
      create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 >>= fun b ->
        let menu_content =
          {{
          [<tr> [
           <td> [
            table ~a:[a_class ["sources_submenu"]] [
             <tr> [
              th ~a:[a_class ["submenu_title"]]
                {: "File" :}
             ]
            <tr> [
              td ~a:[a_class ["sources_menu_item"]]
              [{:
		  Eliom_output.Html5.a
                  ~a:[a_class ["sources_menu_link"]]
		  ~service:ps.Sh.sources_service
		  ~sp (pcdata "View content")}
		  (target,(Some(`Cat),(None,None))):}]
            ]
            <tr> [
              td ~a:[a_class ["sources_menu_item"]]
              [{:
		  Eliom_output.Html5.a
                  ~a:[a_class ["sources_menu_link"]]
		  ~service:ps.Sh.sources_service
		  ~sp (pcdata "File history")}
		  (target,(Some(`Options),(None,None))):}]
              ]
            <tr> [
              td ~a:[a_class ["sources_menu_item"]]
              [{:
		  Eliom_output.Html5.a
                  ~a:[a_class ["sources_menu_link"]]
		  ~service:ps.Sh.sources_service
		  ~sp (pcdata "Annotate")}
		  (target,(Some(`Annot),(None,None))):}]
            ](*
            <tr> [
              td ~a:[a_class ["sources_menu_current"]] {: "File diff" :}]*)
            ]
           ]
          ]]
          }}
        in
        let title_content =
          {{ div ~a:[a_class ["path"]] [ !a
                                  span ~a:[a_class ["last_path_element"]]
                                  {: current_dir :}] }}
        in
        Lwt.return ({{
          	       {{ (sources_page_content
                             sp
                             None
                             ~kind:(Some(`Diff))
                             title_content
                             menu_content
                             b
                             ps) }} }}
                      : {{ Xhtmltypes_duce.flows }})


let draw_patchdiff ~sp ~id ~diff1 ~diff2 = match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
        let menu_content =
          {{ [
             <tr> [
             <td> [
               table ~a:[a_class ["sources_submenu"]] [
                 <tr> [
                   th ~a:[a_class ["submenu_title"]]
                     [ 'Log' ]
                 ]
                 <tr> [
                   td ~a:[a_class ["sources_menu_current"]] {: "Commit diff" :}
                 ]
               ]
             ]
           ]]
           }}
        in
        let title_content =
          {{ div ~a:[a_class ["path"]]
            {: ("Commit diff "^diff1^" - "^diff2) :} }}
        in
        create_patchdiff ~sp ~id ~diff1 ~diff2 >>= fun page_content ->
          Lwt.return ({{
                       {{ sources_page_content
                            sp
                            None
                            ~kind:(Some(`PatchDiff))
                            title_content
                            menu_content
                            page_content
                            ps }} }} : {{ Xhtmltypes_duce.flows }})


(* TODO ¿ cas ou target est un répertoire ? *)
let draw_file_page ~sp ~id ~target ~version ~log_start =
  let str_version = " – version history" in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a =
        path_title ~sp
          ~path:current_dir_path
          ~version
          ~title:""
          ~ps
      in
      create_file_page
        ~sp
        ~id
        ~target
        ~version
        ~log_start
        ~project_services:ps >>= fun (menu_content,page_content) ->
        let title_content =
          {{ div ~a:[a_class ["path"]]
                         [ !a span ~a:[a_class ["last_path_element"]]
                           {: current_dir :}
                           {{ utf8_span None str_version }}]}}
        in
        Lwt.return ({{
		       {{ (sources_page_content
                             sp
                             version
                             ~kind:(Some(`Options))
                             title_content
                             menu_content
                             page_content
                             ps) }} }}
                      : {{ Xhtmltypes_duce.flows }})


let draw_annotate ~sp ~id ~target ~version =
  let file_version = match version with
    | None -> " – latest version"
    | Some(v) -> (" – version "^v)
  in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a =
        path_title
          ~sp
          ~path:current_dir_path
          ~version
          ~title:"Annotate - "
          ~ps
      in
      create_annotate_page
        ~sp
        ~id
        ~target
        ~version
        ~project_services:ps >>= fun b ->
        let menu_content =
          {{
          [<tr> [
           <td> [
            table ~a:[a_class ["sources_submenu"]] [
             <tr> [
              th ~a:[a_class ["submenu_title"]]
                {: "File" :}
             ]
           <tr> [
              td ~a:[a_class ["sources_menu_item"]]
              [{:
                 Eliom_output.Html5.a
                  ~a:[a_class ["sources_menu_link"]]
		  ~service:ps.Sh.sources_service
		   ~sp (pcdata "View content")}
		  (target,(Some(`Cat),(version,None))) :}]
           ]
           <tr> [
            td ~a:[a_class ["sources_menu_item"]]
              [{:
		  Eliom_output.Html5.a
                  ~a:[a_class ["sources_menu_link"]]
		  ~service:ps.Sh.sources_service
		   ~sp (pcdata "File history")}
		  (target,(Some(`Options),(None,None))) :}]
            ]
           <tr> [
              td ~a:[a_class ["sources_menu_current"]] {: "Annotate" :}]
          ]]]] }}
        in
        let title_content =
          {{ div ~a:[a_class ["path"]]
                         [ !a span ~a:[a_class ["last_path_element"]]
                           {: current_dir :}
                           {{ utf8_span None file_version }}]}}
        in
        Lwt.return ({{
                       {{ (sources_page_content
                             sp
                             version
                             ~kind:(Some(`Annot))
                             title_content
                             menu_content
                             b
                             ps) }} }}
                      : {{ Xhtmltypes_duce.flows }})


let draw_wrong_url_page ~sp ~id = match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let menu_content = {{ [] }} in
      let title_content =
        {{ div ~a:[a_class ["path"]] [<span> {:"Error - malformed URL":}] }}
      in
      warning sp "Wrong URL parameters" >>= fun b ->
      Lwt.return ({{
                     {{ (sources_page_content
                           sp
                           None
                           ~kind:(Some(`Error))
                           title_content
                           menu_content
                           b
                           ps) }} }}
                    : {{ Xhtmltypes_duce.flows }})
*)
