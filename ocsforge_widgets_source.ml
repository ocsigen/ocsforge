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

let cut_string s size = 
  if String.length s > size then
    ((String.sub s 0 (size-3))^"...")
  else s

let repository_table_header = 
  ({{ [<tr> [ <th class="sources_table"> []
              <th class="sources_table"> ['File'] 
	      <th class="sources_table"> ['Author']
	      <th class="sources_table"> ['Latest version'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})

(*
let code_table_header file version = 
  ({{ [<tr> [<th class="sources_table"> {: (file) :}]] }} 
	 : {{ [Xhtmltypes_duce.tr] }})
*)

let log_table_header = 
  ({{ [<tr> [ <th class="sources_table"> ['Version']
              <th class="sources_table"> ['Author'] 
	      <th class="sources_table"> ['Date']
	      <th class="sources_table"> ['Comment'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})


let create_repository_table_content ~sp ~id ~version 
    ~project_services =
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cpt = ref 0 in
	  let rec build_content tree current_dir depth = match tree with
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
				  ((if (String.length current_dir != 0) then
				    (Neturl.split_path (current_dir^"/"^(!f)))
				  else [!f]),(version,(true,(None,None))))
				  :}] }}
			      :}
		      {: match project_services with
		          | None -> 
			      {{ <td class="sources_table"> {: (!f) :} }}
			  | Some(ps) -> 
			      {{
			       <td class="sources_table"> 
				 [ {: 
				     let rev = 
				       match version with
				       | None -> Some("latest")
				       | _ -> Some(rev_id)
				     in
				     Eliom_duce.Xhtml.a 
				     ~service:ps.Sh.sources_service 
				     ~sp {{ {: !f :} }}
				     ((if (String.length current_dir != 0) then
				       Neturl.split_path (current_dir^"/"^(!f))
				     else [!f]),(rev,(false,(None,None))))
				     :}] }}
			  :}
			  <td class="small_font_center"> {: aut :}
			  {:
			    begin match project_services with
			    | None -> 
				{{ <td class="small_ifont_center"> {: cut_string (rev_name) 50 :}}}
			    | Some(ps) ->
				{{ <td class="small_ifont_center">
			    	  [{: Eliom_duce.Xhtml.a
				      ~service:ps.Sh.sources_service
				      ~sp {{ {: cut_string rev_name 80 :}  }}
				      ([],(Some(rev_id),(false,(None,None))))
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
		    build_content h dir (depth+1) >>= fun built ->
		      (aux t dir ({{ [ !res !built ] }}))
	      in
	      let new_dir = 
		if (String.length current_dir == 0) then   
		  if (String.compare !d "." != 0) then !d
		  else ""
		else (current_dir^"/"^(!d))
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
		((build_content tree "" (-1)) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  | Some(v) ->
	      fun_pack.STypes.vm_list ~id:v path >>= fun tree -> 
		((build_content tree "" (-1)) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  end
    | (_,_) -> Lwt.return {{ [ <tr> [
			       <td> ['Error: unable to access the repository']]] }}
	  
	      
let create_source_code_content ~sp ~id ~file ~version =
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
	    Ocsforge_color.color (Lexing.from_string (String.concat "\n" l)) file 0 >>= 
	    fun (lines,content) ->
	      Lwt.return 
		({{ [ <div class="lines">[<pre class="left_lines"> {: lines :}]
		      <div class="source_code">[<pre class="color"> {: content :}]
		      <div class="lines">[<pre class="right_lines"> {: lines :}]]}} : {{ [Xhtmltypes_duce.block*] }})
    | (_,_) -> Lwt.return ({{ [ <div>[<pre> ['Error: unable to access the repository']]] }})


let create_log_table_content ~sp ~id ~file ~project_services = 
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
		      {{ <td class="small_font"> {: cut_string !(p.STypes.name) 50 :} }}
		  | Some(ps) ->
		      {{ <td class="small_font"> 
			[{: Eliom_duce.Xhtml.a
			    ~service:ps.Sh.sources_service
			    ~sp {{ {: cut_string !(p.STypes.name) 50 :} }}
			    ([],(Some(!(p.STypes.id)),(false,(None,None))))
			    :}] }}
			:} 
	       <td class="small_font"> {: !(p.STypes.author) :}
	       <td class="xsmall_font"> {: !(p.STypes.date) :}
	       <td class="small_ifont"> {: Ocamlduce.Utf8.make (cut_string !(p.STypes.comment) 50) :}] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let log = match file with
	    | None -> fun_pack.STypes.vm_log path
	    | Some(f) -> fun_pack.STypes.vm_log ~file:f path
	  in
	  log >>= fun log_result -> 
	    extract_result log_result 	
    | (_,_) -> Lwt.return ({{ [ <tr> [<td> ['Error: unable to access the repository']]] }})
      

let log_table_content ~sp ~id ~log ~project_services = 
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
			    ([],(Some(!(p.STypes.id)),(false,(None,None))))
			    :}] }}
			:} 
	       <td class="small_font"> {: !(p.STypes.author) :}
	       <td class="xsmall_font"> {: !(p.STypes.date) :}
	       <td class="small_ifont"> {: Ocamlduce.Utf8.make (cut_string !(p.STypes.comment) 50) :}] !b]}} 
	       :{{ [Xhtmltypes_duce.tr*] }})
  in
  extract_result log


(* TODO *)
let create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 =
  let rec extract_diff_result (span_class:string) (content_list:((STypes.rowType*string) list)) = match content_list with
    | [] -> Lwt.return {{ [] }}
    | (rt,s)::t ->
	extract_diff_result span_class t >>= fun b ->
	  match rt with 
	  | STypes.Common ->
	      Lwt.return ({{ [ <span>{: (s^"\n") :} !b ]  }} : {{ [Xhtmltypes_duce.special_pre*] }})
	  | STypes.Diff ->
	      Lwt.return ({{ [ <span class={: span_class :}>{: (s^"\n") :} !b ]  }} : {{ [Xhtmltypes_duce.special_pre*] }})
	  | STypes.Blank ->
	      Lwt.return ({{ [ <span class="blank">{: (s^"\n") :} !b ]  }} : {{ [Xhtmltypes_duce.special_pre*] }})
  in 
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let (old_file,new_file) = 
	    if (snd (diff1) > snd (diff2)) then (fst diff1,fst diff2)
	    else (fst diff2,fst diff1) 
	  in
	  fun_pack.STypes.vm_diff file path old_file new_file  >>= fun diff_result ->
	    extract_diff_result "old" diff_result.STypes.oldContent >>= fun oldf ->
	      extract_diff_result "new" diff_result.STypes.newContent >>= fun newf -> 
		Lwt.return ({{ [
			       <div class="diff">[
				 <h5> {: ("@ "^old_file) :} 
				   <pre class="diff"> 
				     {:
					oldf
					:}]
				   <div class="diff">[
				     <h5> {: ("@ "^new_file) :}
				       <pre class="diff">
					 {:
					    newf 
					    :}]
			     ] }} : {{ [Xhtmltypes_duce.block*] }})
    | _ -> 
	Lwt.return ({{ [<p> ['Error: unable to access the repository']] }} 
	   : {{ [Xhtmltypes_duce.block*] }})



let rec soption_list_of_list l =
  List.map (fun h -> 
    (Eliom_duce.Xhtml.Option ({{ {} }},(fst h,snd (snd h)),
			      Some(Ocamlduce.Utf8.make (fst (snd h))),false))) l
  
let rec string_soption_list_of_list l =
  List.map (fun h -> 
    (Eliom_duce.Xhtml.Option ({{ {} }},(fst h),
			      Some(Ocamlduce.Utf8.make (fst (snd h))),false))) l


let rec extract_version_assoc_list log =
  let i = ref 0 in
  List.map 
    (fun p -> 
      let name = cut_string !(p.STypes.name) 30 in
      let pos = !i in
      i := !i + 1;
      (!(p.STypes.id),(name,pos))) 
    log

let find_id ~list name = List.assoc name list

(* TODO *)
let create_file_page ~sp ~id ~target ~version ~project_services = 
  (* TODO : determiner si path est un fichier ou répertoire *)
  (* Seul le cas du fichier est traité pour l'instant *)
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let file_path = match target with
	    | [f] -> f
	    | _ -> String.concat "/" target
          in
	  let log_call = match version with
	  | None -> fun_pack.STypes.vm_log ~file:file_path path
	  | Some(v) -> fun_pack.STypes.vm_log ~file:file_path ~id:v path
	  in
	  log_call >>= fun log_result -> 
	    log_table_content ~sp ~id ~log:log_result ~project_services >>= fun log ->
	    let v_list = extract_version_assoc_list log_result in
	    let code_list = string_soption_list_of_list v_list in
	    let diff_list = soption_list_of_list v_list in
	    let file_version_select_form 
		((file,(version,(browse,(diff1,diff2))))
		   : ([`One of string list] Eliom_parameters.param_name *
			([ `One of string ] Eliom_parameters.param_name *
			   ([ `One of bool ] Eliom_parameters.param_name *
			      ([ `One of (string * int)] Eliom_parameters.param_name *
				  [ `One of (string * int)] Eliom_parameters.param_name))))) =
	      {{[ <p> [
		  {:
		     Eliom_duce.Xhtml.user_type_input
		     (Ocsigen_extensions.string_of_url_path ~encode:false)
		     ~input_type: {: "hidden" :}
		     ~name: file
		     ~value: target
		     ()
		     :}
		    'Select a version  '
		    {: 
		       Eliom_duce.Xhtml.string_select
		       ~name: version
		       (List.hd code_list)
                       (List.tl code_list)
                       :}
		    {: 
		       Eliom_duce.Xhtml.button
		       ~button_type: {: "submit" :}
		       {{ "View code" }}
		       :}
		]] }}
	    in
	    let file_diff_form
		((file,(version,(browse,(diff1,diff2))))
		   : ([`One of string list] Eliom_parameters.param_name *
			([ `One of string ] Eliom_parameters.param_name *
			   ([ `One of bool ] Eliom_parameters.param_name *
			      ([ `One of (string * int)] Eliom_parameters.param_name *
				 [ `One of (string * int)] Eliom_parameters.param_name))))) =
	      {{[ <p> [
		  {:
		     Eliom_duce.Xhtml.user_type_input
		     (Ocsigen_extensions.string_of_url_path ~encode:false)
		     ~input_type: {: "hidden" :}
		     ~name: file
		     ~value: target 
		     ()
		     :}
		    (*{:
		       Eliom_duce.Xhtml.bool_input
		       ~input_type: {: "hidden" :}
		       ~name: 
		     :}*)
		    'Version 1 '
		    {: 
		       Eliom_duce.Xhtml.user_type_select
		       Vm.pair_to_string
		       ~name: diff1
		       (List.hd diff_list)
                       (List.tl diff_list)
                       :}
		    <br> []
		    'Version 2 '
		    {: 
		       Eliom_duce.Xhtml.user_type_select
		       Vm.pair_to_string
		       ~name: diff2
		       (List.hd diff_list)
                       (List.tl diff_list)
                       :}
		    (*<br> []*)
		    {: Eliom_duce.Xhtml.button
		       ~button_type: {: "submit" :}
		       {{ "Execute diff" }}
		       :} 
		]] }}
	    in
	    Lwt.return ({{ [
			   !{:
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
				  :}
			     <p>[<br>[]]
			     <table class="sources_table"> [!log_table_header !log]
			 ]  }} : {{ [Xhtmltypes_duce.block*] }})
	      
    | (_,_) -> Lwt.return 
	  ({{ [<p> ['Error: unable to access the repository']] }} : {{ [Xhtmltypes_duce.block*] }})
	 
  

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
					 ([],(None,(false,(None,None)))):}]]}}
			       end
			  :}
		      <p> [<br>[]]
		      <table class="sources_table">  
		      [!repository_table_header !b]] }} 
		  : {{ [ Xhtmltypes_duce.block* ] }})


let draw_source_code_view ~sp ~id ~target ~version =
  let file = match target with
    | [f] -> f  
    | _ -> Ocsigen_extensions.string_of_url_path ~encode:false target 
  in
  let project_services = Sh.find_service id in
  create_source_code_content ~sp ~id ~file ~version >>= fun b ->
    Lwt.return ({{ [  
		   <h3> {: 
                        let s = match version with
			 | None -> "head"
			 | Some(v) -> v
			in (file^" @ "^s) :}
		     !{: 
		      match project_services with
		        | None -> {{ [] }}
			| Some(ps) ->
			    {{ [<div class="sources_div">
			      [{: 
				  Eliom_duce.Xhtml.a
				  ~service:ps.Sh.sources_service
				  ~sp {{ {:"Back to repository content":} }}
				  ([],(None,(false,(None,None)))):}]]}}
			      :}
		     !{:  
		       match project_services with
		       | None -> {{ [] }}
		       | Some(ps) ->
			   {{ [<div class="sources_div">
			   [{:
			       Eliom_duce.Xhtml.a 
			       ~a: {{ {class="sources_img_link"} }}
			       ~service:ps.Sh.sources_service
			       ~sp {{ {: "More options for this file" :}  }}
			       (target,(version,(true,(None,None)))):}] ] }}
			     :}
		       <p> [<br>[]]
		       !b] }} 
		  : {{ [ Xhtmltypes_duce.block* ] }})


let draw_log_table ~sp ~id ~file = 
  let project_services = Sh.find_service id in 
  create_log_table_content ~sp ~id ~file ~project_services >>= fun b ->
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
				  ([],(None,(false,(None,None))))
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
    | _ -> Ocsigen_extensions.string_of_url_path ~encode:false target
  in
  create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 >>= fun b ->
    Lwt.return ({{ [   <h3> {: ("Diff result on "^file) :}
		       !{: 
		       match project_services with
		       | None -> {{ [] }}
		       | Some(ps) ->
			   {{ [<div class="sources_div">
			     [{: 
				 Eliom_duce.Xhtml.a
				 ~service:ps.Sh.sources_service
				 ~sp {{ {:"Back to repository content":} }}
				 ([],(None,(false,(None,None)))):}]]}}
			     :}
			 !{:  
			   match project_services with
			   | None -> {{ [] }}
			   | Some(ps) ->
			       {{ [<div class="sources_div">
				 [{:
				     Eliom_duce.Xhtml.a 
				     ~a: {{ {class="sources_img_link"} }}
				     ~service:ps.Sh.sources_service
				     ~sp {{ {: "More options for this file" :}  }}
				     (target,(None,(true,(None,None)))):}] ] }}
				 :} <p>[<br>[]] !b ]}}  : {{ [ Xhtmltypes_duce.block* ] }})
  
(* TODO ¿ cas ou target est un répertoire ? *)
let draw_file_page ~sp ~id ~target ~version =
  let project_services = Sh.find_service id in
  create_file_page ~sp ~id ~target ~version ~project_services >>= fun b ->
    let filename = String.concat "/" target in
    Lwt.return ({{ [ 
		   <h3> {: ("File browser - "^filename) :}
		   !{: 
		     match project_services with
		     | None -> {{ [] }}
		     | Some(ps) ->
			 {{ [<div class="sources_div">
			   [{: 
			       Eliom_duce.Xhtml.a
			       ~service:ps.Sh.sources_service
			       ~sp {{ {:"Back to repository content":} }}
			       ([],(None,(false,(None,None)))):}]]}}
			   :} <p>[<br>[]]  !b ] }} : {{ [ Xhtmltypes_duce.block* ] }})
      
