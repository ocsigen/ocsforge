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
     
let utf8_td content (tdclass:string) =
  Lwt.try_bind 
    (fun () -> Lwt.return (Ocamlduce.Utf8.make content))
    (fun utf8 -> 
      Lwt.return  
        ({{ [<td class={: tdclass :}> {: utf8 :}] }} 
           : {{ [Xhtmltypes_duce.td] }}))
    (function _ -> 
      Lwt.return ({{ [<td class={: tdclass :}> {: content :}] }} 
                    : {{ [Xhtmltypes_duce.td] }}))
 
let repository_table_header = 
  ({{ [<tr> [ 
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
    
    
let create_repository_table_content ~sp ~id ~version ~dir ~project_services =
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cpt = ref 0 in
	  let rec build_content tree current_dir depth = match tree with
	  | STypes.File(f,aut,(rev_name,rev_id)) ->
              let file_path = match dir with
                | None -> [!f]
                | Some(list_path) -> List.rev ((!f)::(List.rev list_path))
              in
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
				       ["source_file.png"] :}>[] !{: !f :}] }}
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
				(file_path,(version,(true,(false,(false,(None,None))))))
				:}
                           
                               {: let rev = 
                                 match version with
                                 | None -> Some("latest")
				 | _ -> Some(rev_id)
			       in
			       Eliom_duce.Xhtml.a 
                                 ~a: {{ {class="sources_img_link"} }}
				 ~service:ps.Sh.sources_service 
				 ~sp {{ {: !f :} }}
				 (file_path,(rev,(false,(true,(false,(None,None))))))
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
				        ([],(Some(rev_id),(false,(false,(false,(None,None))))))
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
                    if (depth == 0) then 
		      build_content h dir (depth+1) >>= fun built ->
		        (aux t dir ({{ [ !built !res ] }}))
                    else Lwt.return res
	      in
              let dir_path = match dir with
                | None -> [(!d)]
                | Some(list_path) -> List.rev ((!d)::(List.rev list_path))
              in
	      let a =
		if (String.length (!d) > 0) && (depth>0) then
		  {{ [<tr class="folder">[
                      {:
                        begin match project_services with
                         | None -> 
                             {{ <td> [<img alt="folder" 
		                         src={:Eliom_duce.Xhtml.make_uri ~sp
			                        ~service:(Eliom_services.static_dir ~sp)
			                        ["source_folder.png"] :}>[] !{: (!d) :}] }}
                         | Some(ps) ->
                             {{ <td> [{: Eliom_duce.Xhtml.a
                                         ~a: {{ {class="sources_img_link"} }}
				         ~service:ps.Sh.sources_service
				         ~sp 
                                         {{ [<img alt="folder" 
		                                src={:Eliom_duce.Xhtml.make_uri ~sp
			                               ~service:(Eliom_services.static_dir ~sp)
			                               ["source_folder.png"] :}>[] !{: (!d) :}]}}
                                         (dir_path,(version,(false,(false,(false,(None,None))))))
				         :}] }}
                       end :}
		      <td> []
		      <td> []] ] }}
		else {{ [ ] }}
	      in
              let dir_name = !d in
	      (({{aux l dir_name a}}) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  in
          let rep_path = match dir with
            | None -> path
            | Some(l) -> (path^"/"^(String.concat "/" l))
          in
	  begin match (version,dir) with
	  | (None,None) ->
	      fun_pack.STypes.vm_list path >>= fun tree -> 
		((build_content tree "" 0) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  | (Some(v),None) ->
	      fun_pack.STypes.vm_list ~id:v path >>= fun tree -> 
		((build_content tree "" 0) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
          | (None,Some(l)) ->
	      fun_pack.STypes.vm_list ~dir:(String.concat "/" l) path >>= fun tree -> 
		((build_content tree "" 0) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
          | (Some(v),Some(l)) ->
	      fun_pack.STypes.vm_list ~id:v ~dir:(String.concat "/" l) path >>= fun tree -> 
		((build_content tree "" 0) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
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
	    Ocsforge_color.color (Lexing.from_string (String.concat "\n" l)) file >>= 
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
          (utf8_td (cut_string !(p.STypes.comment) 50) "small_ifont") >>= fun comment_td ->
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
			  ([],(Some(!(p.STypes.id)),(false,(false,(false,(None,None))))))
			  :}] }}
		      :} 
	           <td class="small_font"> {: !(p.STypes.author) :}
	           <td class="xsmall_font"> {: !(p.STypes.date) :}
                   !comment_td] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let log = match file with
	    | None -> fun_pack.STypes.vm_log ~limit:100 path 
	    | Some(f) -> fun_pack.STypes.vm_log ~file:f ~limit:100 path
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
          (utf8_td (cut_string !(p.STypes.comment) 50) "small_ifont") >>= fun comment_td ->
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
			  ([],(Some(!(p.STypes.id)),(false,(false,(false,(None,None))))))
			  :}] }}
		      :} 
	       <td class="small_font"> {: !(p.STypes.author) :}
	       <td class="xsmall_font"> {: !(p.STypes.date) :}
	       !comment_td] !b]}} 
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
		((file,(version,(browse,(view,(annot,(diff1,diff2))))))
		   : ([`One of string list] Eliom_parameters.param_name *
			([ `One of string ] Eliom_parameters.param_name *
			   ([ `One of bool ] Eliom_parameters.param_name * 
                              ([ `One of bool ] Eliom_parameters.param_name * 
                                 ([ `One of bool ] Eliom_parameters.param_name *
			            ([ `One of (string * int)] Eliom_parameters.param_name *
				       [ `One of (string * int)] Eliom_parameters.param_name))))))) =
	      {{[ <p> [
		  {:
		     Eliom_duce.Xhtml.user_type_input
		     (Ocsigen_extensions.string_of_url_path ~encode:false)
		     ~input_type: {: "hidden" :}
		     ~name: file
		     ~value: target
		     ()
		     :}
                    {: 
                       Eliom_duce.Xhtml.bool_checkbox
                       ~a: {{ { type="hidden" } }}
                       ~checked: true
                       ~name: view ()
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
		((file,(version,(browse,(view,(annot,(diff1,diff2))))))
		   : ([`One of string list] Eliom_parameters.param_name *
			([ `One of string ] Eliom_parameters.param_name *
			   ([ `One of bool ] Eliom_parameters.param_name *
			      ([ `One of bool ] Eliom_parameters.param_name * 
                                 ([ `One of bool ] Eliom_parameters.param_name * 
                                    ([ `One of (string * int)] Eliom_parameters.param_name *
				       [ `One of (string * int)] Eliom_parameters.param_name))))))) =
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
  (*Printf.sprintf "#%x%x%x%x%x%x" 
    ((r lsr 8) land 0xf)
    (r land 0xf)
    ((g lsr 8) land 0xf)
    (g land 0xf)
    ((b lsr 8) land 0xf)
    (b land 0xf)*)
    
  

let create_annotate_page ~sp ~id ~target ~version ~project_services = 
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
              Lwt.return (({{ [<span style={:("display:block; background-color:"^color):}> 
                                {: aut :}  !a]}},
                           {{ [
                              <span style={:("display:block; background-color:"^color):}> {: utf8 :} 
                                
                                !b] }}) 
                            : ({{ [Xhtmltypes_duce.span*] }}*{{ [Xhtmltypes_duce.span*] }})))
            (function _ ->
              Lwt.return (({{ [<span style={:("display:block; background-color:"^color):}> 
                                {: aut :}  !a]}},
                           {{ [
                              <span style={:("display:block; background-color:"^color):}> {: line :} 
                                
                                !b] }}) 
                            : ({{ [Xhtmltypes_duce.span*] }}*{{ [Xhtmltypes_duce.span*] }})))
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
        Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let file_path = match target with
	    | [f] -> f
	    | _ -> String.concat "/" target
          in
          let annot_command = match version with
            | None -> fun_pack.STypes.vm_annot path file_path 
            | Some(v) -> fun_pack.STypes.vm_annot ~id:v path file_path 
          in
          annot_command >>= fun annot_result -> 
            extract_annotate_result annot_result >>= fun (a,b) ->
              Lwt.return {{ [
                            <div class="annot_author"> [
                                <pre> [!a]
                              ] 
                              <div class="annot_content"> [
                                <pre> [<code>[!b]] 
                              ]
                            ] }}
    | (_,_) -> Lwt.return 
	  ({{ [<p> ['Error: unable to access the repository']] }} : {{ [Xhtmltypes_duce.block*] }})
	 


let draw_repository_table ~sp ~id ~version ~dir =
  let project_services = Sh.find_service id in
  let (title,table) = match (version,dir) with
  | (None,None) -> ("/ - latest version",
                    create_repository_table_content ~sp ~id ~version ~dir:None
                    ~project_services) 
  | (None,Some(rep)) -> 
      let dirname = String.concat "/" rep in
      ((dirname^" - latest version"),
       (create_repository_table_content 
          ~sp ~id ~version 
          ~dir ~project_services))
  | (Some(v),None) -> (("/ - version "^v),
                       create_repository_table_content ~sp ~id ~version 
                         ~dir:None
                         ~project_services)
  | (Some(v),Some(rep)) -> 
       let dirname = String.concat "/" rep in
      ((dirname^" - version "^v),
       create_repository_table_content ~sp ~id ~version 
         ~dir ~project_services)
  in table >>= fun b ->
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
					([],(None,(false,(false,(false,(None,None)))))):}]]}}
			      end
			        :}
		        <p> [<br>[]]
		        <table class="sources_table">  
		          [!repository_table_header 
                             !{:
                              match project_services with
                              | None -> ({{ [] }} : {{ [Xhtmltypes_duce.tr* ] }})
                              | Some(ps) ->
                                  match (version,dir) with
                                  | (_,None) -> ({{ [] }} : {{ [Xhtmltypes_duce.tr* ] }})
                                  | (None,Some(d)) ->
                                      {{ [<tr class="folder"> [
                                         <td> [{:
                                                  Eliom_duce.Xhtml.a 
				                  ~a: {{ {class="sources_img_link"} }}
				                  ~service:ps.Sh.sources_service
                                                  ~sp
                                                  {{ [<img alt="parent_directory" 
		                                       src={:Eliom_duce.Xhtml.make_uri ~sp
			                                      ~service:(Eliom_services.static_dir ~sp)
			                                      ["parent_directory.png"] :}>[] '../'] }} 
                                                  (List.rev (List.tl (List.rev d)),
                                                 (None,(false,(false,(false,(None,None)))))):}]
                                          <td> [] <td> []
                                      ]]}}
                                  | (v,Some(d)) ->
                                      {{ [<tr class="folder"> [
                                        <td> [{:
                                                  Eliom_duce.Xhtml.a 
				                  ~a: {{ {class="sources_img_link"} }}
				                  ~service:ps.Sh.sources_service
                                                  ~sp
                                                  {{ [<img alt="parent_directory" 
		                                       src={:Eliom_duce.Xhtml.make_uri ~sp
			                                      ~service:(Eliom_services.static_dir ~sp)
			                                      ["parent_directory.png"] :}>[] '../'] }} 
                                                  (List.rev (List.tl (List.rev d)),
                                                 (v,(false,(false,(false,(None,None)))))):}]
                                          <td> [] <td> []]]}}
                                        :}
                             !b]] }} 
		  : {{ Xhtmltypes_duce.flows }})
      

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
				  ([],(None,(false,(false,(false,(None,None)))))):}]]}}
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
			       (target,(version,(true,(false,(false,(None,None)))))):}] ] }}
			     :}
		       <p> [<br>[]]
		       !b] }} 
		  : {{ Xhtmltypes_duce.flows }})


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
				  ([],(None,(false,(false,(false,(None,None))))))
				  :}]] }}
			:}
	             <p> [<br>[]]
	             <table class="sources_table">
		     [!log_table_header !b]]}}
		  : {{ Xhtmltypes_duce.flows }})


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
				 ([],(None,(false,(false,(false,(None,None)))))):}]]}}
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
				     (target,(None,(true,(false,(false,(None,None)))))):}] ] }}
				 :} <p>[<br>[]] !b ]}}  : {{ Xhtmltypes_duce.flows }})
  
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
			         ([],(None,(false,(false,(false,(None,None)))))):}]]}}
			     :} <p>[<br>[]]  !b ] }} : {{ Xhtmltypes_duce.flows }})
      

let draw_annotate ~sp ~id ~target ~version =
  let filename = String.concat "/" target in
  let project_services = Sh.find_service id in
  create_annotate_page ~sp ~id ~target ~version ~project_services >>= fun b ->
    Lwt.return ({{ [<h3> {: ("Annotate - "^filename) :}
                       !{: 
		         match project_services with
		         | None -> {{ [] }}
		         | Some(ps) ->
			     {{ [<div class="sources_div">
			       [{: 
				   Eliom_duce.Xhtml.a
				   ~service:ps.Sh.sources_service
				   ~sp {{ {:"Back to repository content":} }}
				   ([],(None,(false,(false,(false,(None,None)))))):}]]}}
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
				   (target,(None,(true,(false,(false,(None,None)))))):}] ] }}
			       :}
                       <p>[<br>[]]
                       !b]}} : {{ Xhtmltypes_duce.flows }})
