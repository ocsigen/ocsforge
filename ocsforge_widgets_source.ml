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

let (>>=) = Lwt.bind
module Data = Ocsforge_data
module Types = Ocsforge_types
module STypes = Ocsforge_source_types
module Vm = Ocsforge_version_managers
module Sh = Ocsforge_services_hashtable

let generate_css_style id css_class =
  if (!id mod 2 == 0) then
    (css_class^"_odd")
  else (css_class^"_even")

let create_repository_table_content ~sp ~id ~version 
    ~project_services =
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cpt = ref 0 in
	  let rec build_content tree current_dir = match tree with
	  | STypes.File(f,aut,(rev_name,rev_id)) ->
	      Lwt.return
                {{ [ <tr class={:
				if (!cpt mod 2 == 0) then begin 
				  cpt := !cpt + 1;
				  "odd"
				end
				else begin
				  cpt := !cpt + 1;
				  "even"
				end
				    :}>
		  [  {:
		      match project_services with
			 | None ->
			     {{ <td class="sources_table"> 
		               [<img alt="file" 
				   src={:Eliom_duce.Xhtml.make_uri ~sp
					  ~service:(Eliom_services.static_dir ~sp)
					  ["source_file.png"] :}>[]] }}
			| Some(ps) ->
			    {{ <td class="sources_table"> 
			      [{:
				  Eliom_duce.Xhtml.a 
				  ~a: {{ {class="sources_img_link"} }}
				  ~service:ps.Sh.sources_service
				  ~sp {{  [<img alt="file" 
					      src={:Eliom_duce.Xhtml.make_uri ~sp
						     ~service:(Eliom_services.static_dir ~sp)
						     ["source_file.png"] :}>[]] }}
				  (([if (String.length current_dir != 0) then
				    (current_dir^"/"^f)
				  else f]),(None,(None,None)))
				  :}] }}
			      :}
		      {: match project_services with
		          | None -> 
			      {{ <td class="sources_table"> {: f :} }}
			  | Some(ps) -> 
			      {{
			       <td class="sources_table"> 
				 [{: 
				     let rev = 
				       match version with
				       | None -> Some("latest")
				       | _ -> Some(rev_id)
				     in
				     Eliom_duce.Xhtml.a 
				     ~service:ps.Sh.sources_service 
				     ~sp {{ {: f :} }}
				     (([if (String.length current_dir != 0) then
				       (current_dir^"/"^f)
				     else f]),(rev,(None,None)))
				     :}] }}
			  :}
			  <td class="small_font"> {: aut :}
			  {:
			    begin match project_services with
			    | None -> 
				{{ <td class="small_ifont"> {: rev_name :}}}
			    | Some(ps) ->
				{{ <td class="small_ifont">
			    	  [{: Eliom_duce.Xhtml.a
				      ~service:ps.Sh.sources_service
				      ~sp {{ {: rev_name :}  }}
				      ([],(Some(rev_id),(None,None)))
				      :}] }}
			    end
			  :}
		   ]]
                 }} 
 	  | STypes.Dir (d, l) ->
	      let rec aux list dir (res : {{ [ Xhtmltypes_duce.tr* ] }}) = 
		match list with
	        | []   -> Lwt.return res
		| h::t -> 
		    build_content h dir >>= fun built ->
		      (aux t dir ({{ [ !res !built ] }}))
	      in
	      let new_dir = 
		if (String.length current_dir == 0) then   
		  if (String.compare d "." != 0) then d
		  else ""
		else (current_dir^"/"^d)
	      in
	      let a = 
		if (String.length new_dir > 0) then
		  {{ [<tr class="folder">
		    [ <td> 
		      [<img alt="folder" 
			  src={:Eliom_duce.Xhtml.make_uri ~sp
				 ~service:(Eliom_services.static_dir ~sp)
				 ["source_folder.png"] :}>[]]
			<td> {: new_dir :}
			<td> []
			<td> []] ] }}
		else {{ [ ] }}
	      in
	      (({{aux l new_dir a}}) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  in
	  begin match version with
	  | None ->
	      fun_pack.STypes.vm_list path >>= fun tree -> 
		((build_content tree "") : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  | Some(v) ->
	      fun_pack.STypes.vm_list ~id:v path >>= fun tree -> 
		((build_content tree "") : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  end
    | (_,_) -> Lwt.return {{ [ <tr> [
			       <td> ['Error: unable to access the repository']]] }}
	  
	      
let create_source_code_content ~sp ~id ~file ~version =
  let rec aux s l = 
    match l with
    | []   -> s
    | h::t -> aux (s^"\n"^h) t
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cat_call = 
	    begin match version with
	    | None -> fun_pack.STypes.vm_cat path file
	    | Some(v) ->
		fun_pack.STypes.vm_cat ~id:v path file
	    end
	  in cat_call >>= fun l -> 
	    Ocsforge_color.color (Lexing.from_string (aux "" l)) file >>= 
	    fun content ->
	      Lwt.return 
		({{ [<tr> [
		      <td> [
		      <pre class="color"> {: content :}
		    ]]]}} : {{ [ Xhtmltypes_duce.tr* ] }})
    | (_,_) -> Lwt.return ({{ [ <tr> [<td> ['Error: unable to access the repository']]] }})


let create_log_table_content ~sp ~id ~project_services = 
  let cpt = ref 0 in
  let rec extract_result log_result = match log_result with
    | [] -> Lwt.return {{ [] }} 
    | p::t ->
	extract_result t >>= fun b ->
	  Lwt.return 
	    ({{ [<tr class={:
			    if (!cpt mod 2 == 0) then begin 
			      cpt := !cpt + 1;
			      "odd"
			    end
			    else begin
			      cpt := !cpt + 1;
			      "even"
			    end
				:}> 
	      [{: match project_services with 
	          | None -> 
		      {{ <td class="small_font"> {: !(p.STypes.name) :} }}
		  | Some(ps) ->
		      {{ <td class="small_font"> 
			[{: Eliom_duce.Xhtml.a
			    ~service:ps.Sh.sources_service
			    ~sp {{ {: !(p.STypes.name):} }}
			    ([],(Some(!(p.STypes.id)),(None,None)))
			    :}] }}
			:} 
	       <td class="small_font"> {: !(p.STypes.author) :}
	       <td class="xsmall_font"> {: !(p.STypes.date) :}
	       <td class="small_ifont"> {: !(p.STypes.comment) :}] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  fun_pack.STypes.vm_log path >>= fun log_result -> 
	    extract_result log_result 	
    | (_,_) -> Lwt.return ({{ [ <tr> [<td> ['Error: unable to access the repository']]] }})
      

(* TODO *)
let create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 =
  Lwt.return ({{ [<h3> [!{: ("Diff entre "
			   ^diff1^" et "
			   ^diff2^" de "
			   ^file) :}]]}} 
		: {{ [Xhtmltypes_duce.block*] }})

(* TODO *)
let create_file_page ~sp ~id ~path ~project_services = 
  (* TODO : determiner si path est un fichier ou répertoire *)
  (* Seul le cas du fichier est traité pour l'instant *)
  let file_version_select_form 
    ((file,(version,(diff1,diff2)))
    : ([`One of string list] Eliom_parameters.param_name *
	([ `One of string ] Eliom_parameters.param_name *
	   ([ `One of string ] Eliom_parameters.param_name *
		 [ `One of string ] Eliom_parameters.param_name)))) =
  {{[ <p> [
      'Select a version  '
        {: Eliom_duce.Xhtml.string_input
           ~name:version
           ~input_type:{: "text" :}
           () :}
      
       {:Eliom_duce.Xhtml.string_button
	   ~name:version
	   ~value:""
           {{ "View code" }}:}]] }}
  in
  let file_diff_form
    ((file,(version,(diff1,diff2)))
    : ([`One of string list] Eliom_parameters.param_name *
	([ `One of string ] Eliom_parameters.param_name *
	   ([ `One of string ] Eliom_parameters.param_name *
		 [ `One of string ] Eliom_parameters.param_name)))) =
    {{[ <p> [
	  'Version 1 '
          {: Eliom_duce.Xhtml.string_input
             ~name:version
             ~input_type:{: "text" :}
             () :}
	  <br> []
	  'Version 2 '
          {: Eliom_duce.Xhtml.string_input
             ~name:version
             ~input_type:{: "text" :}
             () :}
	  <br> []
	  {:Eliom_duce.Xhtml.string_button
	     ~name:version
	     ~value:""
             {{ "Execute diff" }}:}]] }}
    
  in
  Lwt.return ({{ {:
		   match project_services with
		     | None -> {{ [<div> []] }}
                     | Some(ps) ->
			 {{[<div class="file_version_select"> 
			     [ {: Eliom_duce.Xhtml.get_form
				  ~service: ps.Sh.sources_service
				  ~sp  
				  file_version_select_form :} 
			     ]
			     <div class="file_diff_select"> 
			     [ {: Eliom_duce.Xhtml.get_form
				  ~service: ps.Sh.sources_service
				  ~sp  
				  file_diff_form :} 
			     ]
			   ] }} 
			   :}  }} 
		: {{ [Xhtmltypes_duce.block*] }})

let repository_table_header = 
  ({{ [<tr> [ <th class="sources_table"> []
              <th class="sources_table"> ['File'] 
	      <th class="sources_table"> ['Author']
	      <th class="sources_table"> ['Latest version'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})


let code_table_header file version = 
  ({{ [<tr> [ <th class="code_table"> {: 
	       let s = match version with
	       | None -> "head"
	       | Some(v) -> v
	       in
	       (file^"@"^s) :}]] }} 
	 : {{ [Xhtmltypes_duce.tr] }})

let log_table_header = 
  ({{ [<tr> [ <th class="sources_table"> ['Version']
              <th class="sources_table"> ['Author'] 
	      <th class="sources_table"> ['Date']
	      <th class="sources_table"> ['Comment'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})
  

let draw_repository_table ~sp ~id ~version  =
  let project_services = Sh.find_service id in
  create_repository_table_content ~sp ~id ~version 
    ~project_services >>= fun b ->
      let title = match version with
      | None -> "Repository - latest version"
      | Some(v) -> ("Repository - version "^v)
      in
      Lwt.return ({{  [<h3> {: title :}
		       !{: 
			 match project_services with
			 | None -> {{ [] }}
			 | Some(ps) ->
			     {{ [<div class="sources_div">
			       [{: 
				   Eliom_duce.Xhtml.a 
				   ~service:ps.Sh.log_service
				   ~sp {{['View repository history']}}
				   () :}]]}}
			 :}
			 !{: 
			   match version with
		           | None -> {{ [] }}
			   | Some(_) ->
			       begin match project_services with
			       | None -> {{ [] }}
			       | Some(ps) ->
				   {{ [<div class="sources_div">
				     [{: 
					 Eliom_duce.Xhtml.a
					 ~service:ps.Sh.sources_service
					 ~sp {{ {:"Back to latest" 
                                                 ^" repository version":} }}
					 ([],(None,(None,None))):}]]}}
			       end
			  :}
		      <p> [<br>[]]
		      <table class="sources_table">  
		      [!repository_table_header !b]] }} 
		  : {{ [ Xhtmltypes_duce.block* ] }})


let draw_source_code_view ~sp ~id ~target ~version =
  let file = match target with
    | [f] -> f  
    | _ -> ""
  in
  create_source_code_content ~sp ~id ~file ~version >>= fun b ->
    Lwt.return ({{ [<table class="code_table"> 
                   [!(code_table_header file version) !b]] }} 
		  : {{ [ Xhtmltypes_duce.block* ] }})


let draw_log_table ~sp ~id = 
  let project_services = Sh.find_service id in 
  create_log_table_content ~sp ~id ~project_services >>= fun b ->
    Lwt.return ({{ [
		    !{:
		       match project_services with
		       | None -> {{ [<h3> ['Repository history']] }}
		       | Some(ps) ->
			   {{
			    [<h3> ['Repository history']
			    <div class="sources_div">
			      [{: 
				  Eliom_duce.Xhtml.a
				  ~service:ps.Sh.sources_service
				  ~sp {{ ['Back to repository content'] }}
				  ([],(None,(None,None)))
				  :}]] }}
			:}
	             <p> [<br>[]]
	             <table class="sources_table">
		     [!log_table_header !b]]}}
		  : {{ [ Xhtmltypes_duce.block* ] }})


(* TODO *)
let draw_diff_view ~sp ~id ~target ~diff1 ~diff2 =
  let project_services = Sh.find_service id in
  let file = match target with
    | [f] -> f
    | _ -> "" 
  in
  create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 >>= fun b ->
    Lwt.return ({{ [ !b ]}}  : {{ [ Xhtmltypes_duce.block* ] }})
  
(* TODO *)
let draw_file_page ~sp ~id ~target =
  let project_services = Sh.find_service id in
  let path = match target with
    | [f] -> f
    | _ -> String.concat "/" target
  in
  create_file_page ~sp ~id ~path ~project_services >>= fun b ->
    Lwt.return ({{ [ !b ] }} : {{ [ Xhtmltypes_duce.block* ] }})
