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

let create_repository_table_content ~sp ~id ~version 
    ~(temp_source_service :
        ((Ocsforge_types.task * string list, unit,
          [ `Attached of
            Eliom_services.get_attached_service_kind Eliom_services.a_s ],
	  [ `WithSuffix ],
	  [ `One of Ocsforge_types.task ] Eliom_parameters.param_name *
	    [ `One of string list] Eliom_parameters.param_name, unit,
	  [ `Registrable ])
	   Eliom_services.service)) = 
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cpt = ref 0 in
	  let rec build_content tree current_dir = match tree with
	  | STypes.File(f,aut,rev) ->
	      (*let file_URL = 
		if String.length current_dir == 0 then f
		else current_dir^"/"^f
		in*)
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
		  [  <td class="sources_table"> [<img alt="file" 
						    src={:Eliom_duce.Xhtml.make_uri ~sp
							   ~service:(Eliom_services.static_dir ~sp)
							   ["source_file.png"] :}>[]]
		      <td class="sources_table"> 
			[{: Eliom_duce.Xhtml.a 
			    ~service:temp_source_service 
			    ~sp {{ {: f :} }}
			    (id,
			     let file_path =
			     if (String.length current_dir != 0) then
			       (current_dir^"/"^f)
			     else f in
			     match version with
			     | None -> [file_path]
			     | Some(v) -> [file_path;v])
			    :}]
			  <td class="sources_table">{: aut :}
			      <td class="sources_table">{: rev :}
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
		    [ <td> [<img alt="folder" 
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


let create_log_table_content ~sp ~id ~src_service = 
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
	      [<td class="sources_table"> 
		[{: Eliom_duce.Xhtml.a
		   ~service:src_service
		   ~sp {{ {: !(p.STypes.name):}  }}
		   (id, !(p.STypes.id))		   
		:}] 
	       <td class="sources_table"> {: !(p.STypes.author) :}
	       <td class="sources_table"> {: !(p.STypes.date) :}
	       <td class="sources_table"> {: !(p.STypes.comment) :}] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  fun_pack.STypes.vm_log path >>= fun log_result -> 
	    extract_result log_result 	
    | (_,_) -> Lwt.return ({{ [ <tr> [<td> ['Error: unable to access the repository']]] }})
      
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
  

let draw_repository_table ~sp ~id ~version ~src_service ~log_service =
  create_repository_table_content ~sp ~id ~version ~temp_source_service:src_service >>= fun b ->
    Lwt.return ({{  [<div class="sources_div">
		       [{: 
			Eliom_duce.Xhtml.a 
			~service:log_service
			~sp {{ ['View repository history'] }}
			(id)
			:}]
	              <p> [<br>[]]
		      <table class="sources_table">  
		      [!repository_table_header !b]] }} 
		  : {{ [ Xhtmltypes_duce.block* ] }})


let draw_source_code_view ~sp ~id ~file ~version =
  create_source_code_content ~sp ~id ~file ~version >>= fun b ->
    Lwt.return ({{ [<table class="code_table"> 
      [!(code_table_header file version) !b]] }} : {{ [ Xhtmltypes_duce.block* ] }})


let draw_log_table ~sp ~id ~void_service = 
  let src_service = match Ocsforge_services_hashtable.find_service id with
  | None -> void_service
  | Some(service) -> service
  in
  create_log_table_content ~sp ~id ~src_service >>= fun b ->
    Lwt.return ({{ [<div class="sources_div">
		       [{: 
			
			(*| None ->
			    Eliom_duce.Xhtml.a
			      ~service:void_service
			      ~sp {{ ['Error : repository service not found'] }}
			      (id, "")*)
			(*| Some(service) ->*)
			    Eliom_duce.Xhtml.a
			      ~service:src_service
			      ~sp {{ ['Back to repository content'] }}
			      (id, "")
			      :}]
	             <p> [<br>[]]
	             <table class="sources_table">
		     [!log_table_header !b]]}}
		  : {{ [ Xhtmltypes_duce.block* ] }})
