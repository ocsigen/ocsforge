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
    
let _PAGE_SIZE = 100

type page_kind = [
    `Latest_version_list
  | `Older_version_list
  | `Log
  | `Diff
  | `Cat
  | `Annot
  | `Options ]


let generate_css_style id css_class =
  if (!id mod 2 == 0) then
    (css_class^"_odd")
  else (css_class^"_even")

let generate_menu sp page_kind content services = 
  {{ <table class="sources_menu"> 
    [ <tr class="sources_menu"> [
      {{ match page_kind with
         | `Latest_version_list -> 
             {{ <td class="sources_menu_current"> 
                  {: "Latest repository version ":} }}
         | _ ->
             {{ <td class="sources_menu_item"> 
	       [{: 
	           Eliom_duce.Xhtml.a
                   ~a: {{ {class="sources_menu_link"} }}
	           ~service:services.Sh.sources_service
	           ~sp {{ {:"Latest repository version":} }}
	           ([],(None,
                        (false,
                  (None,
                   (false,
                    (false,(None,None))))))):}] }} }}]
        <tr class="sources_menu"> [
          {{ match page_kind with
          | `Log -> 
              {{ <td class="sources_menu_current"> 
                  {: "Repository history":} }}
          | _ ->
          {{ <td class="sources_menu_item"> 
            [{: 
		Eliom_duce.Xhtml.a 
                ~a: {{ {class="sources_menu_link"} }}
		~service:services.Sh.log_service
		~sp {{ {: "Repository history" :} }}
		(None) :}] }} }}]
        !content] }} 
        

let sources_page_content sp ~kind menu_content main_content services = 
  {{<div class="sources_main_div">
    [
     <div class="sources_menu_div">
       [ <span class="menu_title"> ['Menu']
         {{generate_menu sp kind menu_content services }} ]
     <div class="sources_content_div"> main_content
   ] }}


let error (message:string) = 
  Lwt.return {{ [<div class="error_message"> {: message :}] }}


let cut_string s size = 
  if String.length s > size then
    ((String.sub s 0 (size-3))^"...")
  else s

let utf8_span (spanclass:(string option)) s = 
  try
    match spanclass with 
    | None -> {{ <span> {: (Ocamlduce.Utf8.make s) :}  }}
    | Some(sc) -> {{ <span class={: sc :}> {: (Ocamlduce.Utf8.make s) :}  }}
  with _ -> 
    match spanclass with
    | None -> {{ <span> {: s :} }}
    | Some(sc) -> {{ <span class={: sc :}> {: s :} }}

let rec utf8_lines l = match l with
| [] -> ({{ [] }} : {{ Xhtmltypes_duce.flows }})
| h::t -> 
    let b = utf8_lines t in
    ({{ [{{ utf8_span None h }} <br> [] !b] }} : {{ Xhtmltypes_duce.flows }})

let utf8_td content (tdclass:string) =
  Lwt.try_bind 
    (fun () -> 
      let s = Netstring_pcre.split (Netstring_pcre.regexp "\n") content in
      Lwt.return (utf8_lines s)
      (*Lwt.return (Ocamlduce.Utf8.make content)*))
    (fun utf8 -> 
      Lwt.return  
        ({{ [<td class={: tdclass :}> utf8 ] }} 
           : {{ [Xhtmltypes_duce.td] }}))
    (function _ -> 
      Lwt.return ({{ [<td class={: tdclass :}> {: content :}] }} 
                    : {{ [Xhtmltypes_duce.td] }}))

let build_path path = match path with
   | [] -> ("",[])
   | [f] -> (f,[])
   | _ -> let l = List.rev path in (List.hd l,List.tl l)
   
   
let rec path_title ~sp ~path ~version ~title ~ps = match path with
| [] ->
    {{ [<span> {: (title) :}
           <span> [{:  Eliom_duce.Xhtml.a
                      ~a: {{ {class="path"} }}
                      ~service:ps.Sh.sources_service
                      ~sp {{ {:"root":} }}
                      ([],(version,(false,(None,(false,(false,(None,None))))))) 
                      :}] <span class="path_delimiter"> ['/']] }} 
| h::t -> 
    let b = path_title ~sp ~path:t ~version ~title ~ps in 
    ({{ [!b <span> [{: Eliom_duce.Xhtml.a
                       ~a: {{ {class="path"} }}
		       ~service:ps.Sh.sources_service
		       ~sp {{ {: h :} }}
		       ((List.rev path),(version,(false,(None,(false,(false,(None,None))))))) 
                       :}] <span class="path_delimiter"> ['/']] }} : {{ Xhtmltypes_duce.flows  }})
        
 
let repository_table_header = 
  ({{ [<tr> [ 
        <th class="sources_table"> ['File'] 
	    <th class="sources_table"> ['Author']
	        <th class="sources_table"> ['Latest version'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})
    

let log_table_header = 
  ({{ [<tr> [ <th class="sources_table"> ['Version']
      <th class="sources_table"> ['Author'] 
	  <th class="sources_table"> ['Date']
	      <th class="sources_table"> ['Comment'] ] ]  }} 
     : {{ [Xhtmltypes_duce.tr] }})
    
    
let create_repository_table_content ~sp ~id ~version ~dir ~project_services =
  let ps = project_services in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let cpt = ref 0 in
	  let rec build_content tree depth = match tree with
	    | STypes.File(f,aut,(rev_name,rev_id)) ->
                let file_path = match dir with
                | None -> [f]
                | Some(list_path) -> List.rev ((f)::(List.rev list_path))
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
		    [ <td class="sources_table"> 
		      [{: let args = match version with 
                            | None -> (file_path,(version,(true,(None,(false,(false,(None,None)))))))
                            | _ -> (file_path,(version,(true,(Some(None,version),(false,(false,(None,None)))))))
                          in
			  Eliom_duce.Xhtml.a 
			  ~a: {{ {class="sources_img_link"} }}
			  ~service:ps.Sh.sources_service
			  ~sp {{  [<img alt="file" 
				      src={:Eliom_duce.Xhtml.make_uri ~sp
					     ~service:(Eliom_services.static_dir ~sp)
					     ["source_file.png"] :}>[]] }}
                          args
			  
			  :}
                         ' '
                         {: (*let rev = 
                              match version with
                              | None -> Some("latest")
			      | Some(v) -> Some(rev_id)
			      in*)
                            Eliom_duce.Xhtml.a 
                            ~a: {{ {class="sources_img_link"} }}
			    ~service:ps.Sh.sources_service 
			    ~sp {{ {: f :} }}
			    (file_path,(version,(false,(None,(true,(false,(None,None)))))))
			    :}] 
			
		        <td class="small_font_center"> {: aut :}
		            <td class="small_ifont_center">
			      [{: Eliom_duce.Xhtml.a
			          ~service:ps.Sh.sources_service
			          ~sp {{ {: cut_string rev_name 200 :}  }}
			          ([],(Some(rev_id),(false,(None,(false,(false,(None,None)))))))
			          :}]
                    ]]
                   }} 
 	    | STypes.Dir (d, l) ->
	        let rec aux list dir (res : {{ [ Xhtmltypes_duce.tr* ] }}) = 
		  match list with
	          | []   -> Lwt.return res
		  | h::t -> 
                      if (depth == 0) then 
		        build_content h (depth+1) >>= fun built ->
		          (aux t dir ({{ [ !built !res ] }}))
                      else Lwt.return res
	        in
                let dir_path = match dir with
                | None -> [d]
                | Some(list_path) -> List.rev ((d)::(List.rev list_path))
                in
	        let a =
		  if (String.length (d) > 0) && (depth>0) then
		    {{ [<tr class="folder">[
                      <td> [{: Eliom_duce.Xhtml.a
                               ~a: {{ {class="sources_img_link"} }}
			       ~service:ps.Sh.sources_service
			       ~sp 
                               {{ [<img alt="folder" 
		                      src={:Eliom_duce.Xhtml.make_uri ~sp
			                     ~service:(Eliom_services.static_dir ~sp)
			                     ["source_folder.png"] :}>[] !{: (" "^(d)) :}]}}
                               (dir_path,(version,(false,(None,(false,(false,(None,None)))))))
			       :}]
		        <td> []
		        <td> []]] }}
		  else {{ [ ] }}
	        in
                let dir_name = d in
	        (({{aux l dir_name a}}) : {{ [ Xhtmltypes_duce.tr* ] }} Lwt.t)
	  in
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
              build_content tree 0 >>= fun b ->
              Lwt.return
                {{  [<table class="sources_table">  
	          [!repository_table_header 
                     !{:
                       match (version,dir) with
                       | (_,None) -> ({{ [] }} : {{ [Xhtmltypes_duce.tr* ] }})
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
			                            ["parent_directory.png"] :}>[] ' ../'] }} 
                                      (List.rev (List.tl (List.rev d)),
                                       (v,(false,(None,(false,(false,(None,None))))))):}]
                               <td> [] <td> []
                           ]]}}
                             :}
                    !b ]] }})
             (function 
               | Vm.Node_not_found -> error "Error: node not found"
               | Vm.Revision_not_found -> error "Error: revision not found"
               | _ -> error "Version manager internal error")
    | _ -> failwith "Unable to retrieve repository informations"
    
	  
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
	  in 
          Lwt.try_bind 
            (fun () -> cat_call)
            (fun s -> 
	      Ocsforge_color.color (Lexing.from_string s) file >>= 
	      fun (lines,content) ->
	        Lwt.return 
		  ({{ [ <div class="source_code_container"> [
                    <div class="left_lines">[<pre class="left_lines"> {: lines :}]
		        <div class="source_code">[<pre class="color"> {: content :}]
		            <div class="right_lines">[<pre class="right_lines"> {: lines :}]
                     ]]}} : {{ [Xhtmltypes_duce.block*] }}))
            (function 
               | Vm.Node_not_found -> error "Error: node not found"
               | Vm.Revision_not_found -> error "Error: revision not found"
               | _ -> error "Version manager internal error")
    | (_,_) -> failwith "Unable to retrieve repository informations"


let rec list_after l n = match l with
 | [] -> []
 | _::t -> 
     if (n = 0) then l 
     else list_after t (n-1)


let rec extract_version_assoc_list log =
  let i = ref 0 in
  List.map 
    (fun p -> 
      let name = cut_string !(p.STypes.name) 30 in
      let pos = !i in
      i := !i + 1;
      (!(p.STypes.id),(name,pos))) 
    log


let rec create_log_select ~log_result ~page_size = match log_result with
  | [] -> []
  | _ ->
      let select_start = List.hd log_result in      
      let select_end = 
        if (List.length log_result >= page_size) then
          List.nth (log_result) (page_size-1)
        else
          List.hd (List.rev log_result) 
      in
      let msg = ((cut_string (fst (snd select_start)) 25)^" -> "^(cut_string (fst (snd select_end)) 25)) in
      let opt = (msg,((fst select_start),(fst select_end))) in
      if (List.length log_result <= page_size) then [opt]
      else
        opt::
        (create_log_select ~log_result:(list_after log_result page_size) ~page_size)


let create_log_links ~sp ~log_service ~log_select ~start_rev =
  let (a,b,c) = 
    if (List.length log_select = 1) then
      (("",("","")),(List.nth log_select 0),("",("","")))
    else if (List.length log_select = 2) then
      match start_rev with
      | None ->
          (("",("","")),List.nth log_select 0,List.nth log_select 1)
      | Some(r) ->
          if (fst (snd (List.nth log_select 0)) = r) then
            (("",("","")),List.nth log_select 0,List.nth log_select 1)
          else
            (List.nth log_select 0,List.nth log_select 1,("",("","")))
    else match start_rev with
    | None ->
        (("",("","")),List.nth log_select 0,List.nth log_select 1) 
    | _ ->
      (List.nth log_select 0,List.nth log_select 1,List.nth log_select 2) 
  in
  (utf8_td (fst b) "middle") >>= fun middle ->
  match start_rev with
    | None -> 
        Lwt.return ({{ 
                     <table class="log_links"> [
                       <tr> [
                       <td class="no_previous_entries"> ['< previous']
                           !middle
                           {{ match c with
                           | ("",_) ->
                               {{ <td class="no_next_entries"> [' next >'] }}
                           | _ ->
                               {{ <td class="next_entries_link">  
                                   [{:Eliom_duce.Xhtml.a
                                        ~a: {{ {class="log_link"} }}
		                        ~service:log_service
		                        ~sp 
                                        {{ ['next >'] }}
                                        (Some(Some(fst (snd c)),Some(snd (snd c)))) :}] 
                                  }} }} ]] }})
    | Some(_) ->
        Lwt.return 
        ({{ 
          <table class="log_links"> [
            <tr class="log_links"> [
            {{
               match a with
               | ("",_) -> {{ <td class="no_previous_entries"> ['< previous'] }}
               | _ -> 
                   {{ <td class="previous_entries_link"> 
                     [{:Eliom_duce.Xhtml.a
                         ~a: {{ {class="log_link"} }}
		         ~service:log_service
		         ~sp 
                         {{ ['< previous'] }}
                         (Some(Some(fst (snd a)),Some(snd (snd a)))) :}]
                                            }} }}
                !middle
                {{ 
                 match c with
                 | ("",_) -> {{ <td class="no_next_entries">  ['next >'] }}
                 | _ -> 
                     {{ <td class="next_entries_link"> [{:Eliom_duce.Xhtml.a
                            ~a: {{ {class="log_link"} }}
		            ~service:log_service
		            ~sp 
                            {{ ['next >'] }}
                            (Some(Some(fst (snd c)),Some(snd (snd c)))) :}]
                      }} }}]] }})
      

(* Unused *)
let log_range_form 
    ~log_select 
    (range : [ `One of (string option *string option)] 
       Eliom_parameters.param_name) = 
  {{ [<p> [
       {: 
	  Eliom_duce.Xhtml.user_type_select
	  Vm.range_to_string
	  ~name: range
	  (List.hd log_select)
          (List.tl log_select)
          :}
	 {: Eliom_duce.Xhtml.button
	    ~button_type: {: "submit" :}
	    {{ "View log" }}
            :}
     ]] }}
            
  
        
let create_log_table_content ~sp ~id ~file ~range ~project_services = 
  let cpt = ref 0 in
  let ps = project_services in
  let (start_rev,end_rev) = match range with
  | None -> (None,None) 
  | Some(r) -> (fst r ,snd r)
  in
  let rec extract_result log_result sr er started limit = match log_result with
    | [] -> Lwt.return {{ [] }} 
    | p::t ->
        if (started = false) then
          if (!(p.STypes.id) = sr) then 
            extract_result t sr er true (limit-1) >>= fun b ->
              (utf8_td (cut_string !(p.STypes.comment) 70) "small_ifont") >>= fun comment_td ->
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
	            [ <td class="small_font"> 
		      [{: Eliom_duce.Xhtml.a
			  ~service:ps.Sh.sources_service
			  ~sp {{ {: cut_string !(p.STypes.name) 50 :} }}
			  ([],(Some(!(p.STypes.id)),(false,(None,(false,(false,(None,None)))))))
			  :}]
		        <td class="small_font"> {: !(p.STypes.author) :}
	                    <td class="xsmall_font"> {: !(p.STypes.date) :}
                                !comment_td] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
          else extract_result t sr er false limit
        else
          if (!(p.STypes.id) = er || limit = 1) then begin
            (utf8_td (cut_string !(p.STypes.comment) 70) "small_ifont") >>= fun comment_td ->
              Lwt.return ({{ [
                             <tr class={:
			                if (!cpt mod 2 == 0) then begin 
			                  cpt := !cpt + 1;
			                  "odd"
			                end
			                else begin
			                  cpt := !cpt + 1;
			                "even"
			                end
				            :}> 
	                       [ <td class="small_font"> 
		                 [{: Eliom_duce.Xhtml.a
		                     ~service:ps.Sh.sources_service
		                     ~sp {{ {: cut_string !(p.STypes.name) 50 :} }}
			             ([],(Some(!(p.STypes.id)),(false,(None,(false,(false,(None,None)))))))
			             :}]
		                   <td class="small_font"> {: !(p.STypes.author) :}
	                               <td class="xsmall_font"> {: !(p.STypes.date) :}
                                           !comment_td] ]}} :{{ [Xhtmltypes_duce.tr*] }})
          end else
	    extract_result t sr er started (limit-1) >>= fun b ->
              (utf8_td (cut_string !(p.STypes.comment) 70) "small_ifont") >>= fun comment_td ->
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
	            [ <td class="small_font"> 
		      [{: Eliom_duce.Xhtml.a
			  ~service:ps.Sh.sources_service
			  ~sp {{ {: cut_string !(p.STypes.name) 50 :} }}
			  ([],(Some(!(p.STypes.id)),(false,(None,(false,(false,(None,None)))))))
			  :}]
		        <td class="small_font"> {: !(p.STypes.author) :}
	                    <td class="xsmall_font"> {: !(p.STypes.date) :}
                                !comment_td] !b]}} :{{ [Xhtmltypes_duce.tr*] }})
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let log = match (file,range) with
	    | (None,None) -> fun_pack.STypes.vm_log ~limit:_PAGE_SIZE path 
	    | (Some(f),None) -> fun_pack.STypes.vm_log ~file:f ~limit:_PAGE_SIZE path
            | (None,Some(r)) -> fun_pack.STypes.vm_log ~range:r ~limit:_PAGE_SIZE path
            | (Some(f),Some(r)) -> fun_pack.STypes.vm_log ~file:f ~range:r ~limit:_PAGE_SIZE path
	  in
          Lwt.try_bind
	  (fun () -> log)
          (fun log_result ->  
            let assoc_list = extract_version_assoc_list log_result in
            let log_select = 
              create_log_select 
                ~log_result:assoc_list
                ~page_size:_PAGE_SIZE
             in 
             let log_table_content = match (start_rev,end_rev) with
             | (None,None) ->
                 extract_result log_result "" "" true _PAGE_SIZE
             | (None,Some(er)) ->
                 extract_result log_result "" er false _PAGE_SIZE
             | (Some(sr),None) ->
                 extract_result log_result sr "" true _PAGE_SIZE
             | (Some(sr),Some(er)) ->  
	         extract_result log_result sr er false _PAGE_SIZE
             in log_table_content >>= fun b ->
               create_log_links ~sp ~log_service:ps.Sh.log_service ~log_select ~start_rev >>= fun (linktable) ->
               Lwt.return {{ [ 
                             (*<div class="file_version_select">
                               [ {:Eliom_duce.Xhtml.get_form
			            ~service: ps.Sh.log_service
			            ~sp  
			            (log_range_form ~log_select)
                                    :}]*)
                                 linktable
                                 <table class="log_table">
		                   [!log_table_header !b]] }})
            (function 
               | Vm.Node_not_found -> error "Error: node not found"
               | Vm.Revision_not_found -> error "Error: revision not found"
               | _ -> error "Version manager internal error")
    | _ -> failwith "Unable to retrieve repository informations"
      

let log_table_content ~sp ~log ~project_services = 
  let cpt = ref 0 in
  let ps = project_services in
  let rec extract_result log_result = match log_result with
    | [] -> Lwt.return {{ [] }} 
    | p::t ->
	extract_result t >>= fun b ->
          (utf8_td (cut_string !(p.STypes.comment) 70) "small_ifont") >>= fun comment_td ->
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
	        [  <td class="small_font"> 
		  [{: Eliom_duce.Xhtml.a
		      ~service:ps.Sh.sources_service
		      ~sp {{ {: !(p.STypes.name):} }}
		      ([],(Some(!(p.STypes.id)),(false,(None,(false,(false,(None,None)))))))
		      :}] 
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
	  let span = begin match rt with 
	  | STypes.Common ->
              utf8_span None (s^"\n")
	  | STypes.Diff ->
              utf8_span (Some(span_class)) (s^"\n")
	  | STypes.Blank ->
              utf8_span (Some("blank")) (s^"\n")
	  end 
          in 
          Lwt.return ({{ [ span !b ]  }} : {{ [Xhtmltypes_duce.special_pre*] }})
  in 
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let (old_file,new_file) = 
	    if (snd (diff1) > snd (diff2)) then (fst diff1,fst diff2)
	    else (fst diff2,fst diff1) 
	  in
	  Lwt.try_bind
            (fun () -> fun_pack.STypes.vm_diff file path old_file new_file)
            (fun diff_result ->
	      extract_diff_result "old" diff_result.STypes.oldContent >>= fun oldf ->
	        extract_diff_result "new" diff_result.STypes.newContent >>= fun newf -> 
		  Lwt.return ({{ [
			         <div class="diff">[
				   <span class="diff_title"> {: ("@ "^old_file) :} 
				       <pre class="diff"> 
				         {:
					oldf
					    :}]
				     <div class="diff">[
				       <span class="diff_title"> {: ("@ "^new_file) :}
				           <pre class="diff">
					     {:
					        newf 
					        :}]
			       ] }} : {{ [Xhtmltypes_duce.block*] }}))
             (function 
               | Vm.Node_not_found -> error "Error: node not found"
               | Vm.Revision_not_found -> error "Error: revision not found"
               | _ -> error "Version manager internal error")
            
    | _ -> failwith "Unable to retrieve repository informations"
	



let rec soption_list_of_list l =
  List.map (fun h -> 
    (Eliom_duce.Xhtml.Option ({{ {} }},(fst h,snd (snd h)),
			      Some(Ocamlduce.Utf8.make (fst (snd h))),false))) l
  
let rec string_soption_list_of_list l =
  List.map (fun h -> 
    (Eliom_duce.Xhtml.Option ({{ {} }},(fst h),
			      Some(Ocamlduce.Utf8.make (fst (snd h))),false))) l


let find_id ~list name = List.assoc name list

let create_file_page ~sp ~id ~target ~range ~project_services = 
  (* TODO : determiner si path est un fichier ou répertoire *)
  (* Seul le cas du fichier est traité pour l'instant *)
  let ps = project_services in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
	Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
	  let file_path = String.concat "/" target in
          let log_call = fun_pack.STypes.vm_log ~file:file_path ~range ~limit:_PAGE_SIZE path
	  in
          Lwt.try_bind
          (fun () -> log_call)
	  (fun log_result -> 
	    log_table_content ~sp ~log:log_result ~project_services >>= fun log ->
	      let v_list = extract_version_assoc_list log_result in
	      let code_list = string_soption_list_of_list v_list in
	      let diff_list = soption_list_of_list v_list in
	      let file_version_select_form ~annotate
		  ((file,(version,(_,(_,(view,(annot,(_,_)))))))
		     : ([`One of string list] Eliom_parameters.param_name *
			  ([ `One of string ] Eliom_parameters.param_name *
			     ([ `One of bool ] Eliom_parameters.param_name * 
                                ([ `One of string option * string option ] Eliom_parameters.param_name *
                                   ([ `One of bool ] Eliom_parameters.param_name * 
                                      ([ `One of bool ] Eliom_parameters.param_name *
			                 ([ `One of (string * int)] Eliom_parameters.param_name *
				            [ `One of (string * int)] Eliom_parameters.param_name)))))))) =
                {{ [ <p class="menu_form"> 
                   [  <span class="menu_form_title">['Content viewing'] 
                      <br> []
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
                         ~name: (if (annotate) then annot else view) ()
                         :}
                      'Select a version  '
		      !{: match code_list with
                      | [] -> {{ [] }}
                      | _ -> {{ [{:
                                    Eliom_duce.Xhtml.string_select
                                    ~a: {{ {class="version_select"} }}
		                    ~name: version
		                    (List.hd code_list)
                                    (List.tl code_list)
                                    :}] }}
                            :}
                      
		      {: 
		         Eliom_duce.Xhtml.button
		         ~button_type: {: "submit" :}
		         {{{: if (annotate) then "Annotate" else "View code" :}}}
		         :}
		  ]] }}
	      in
	      let file_diff_form
		  ((file,(_,(_,(_,(_,(_,(diff1,diff2)))))))
		     : ([`One of string list] Eliom_parameters.param_name *
			  ([ `One of string ] Eliom_parameters.param_name *
			     ([ `One of bool ] Eliom_parameters.param_name *
                                ([ `One of string option * string option ] Eliom_parameters.param_name *
			           ([ `One of bool ] Eliom_parameters.param_name * 
                                      ([ `One of bool ] Eliom_parameters.param_name * 
                                         ([ `One of (string * int)] Eliom_parameters.param_name *
				            [ `One of (string * int)] Eliom_parameters.param_name)))))))) =
                {{[<p class="menu_form">
                    [ <span class="menu_form_title"> ['Diff viewing'] 
                    <br>[]
		    {:
		       Eliom_duce.Xhtml.user_type_input
		       (Ocsigen_extensions.string_of_url_path ~encode:false)
		       ~input_type: {: "hidden" :}
		       ~name: file
		       ~value: target 
		       ()
		       :}
		      'Version 1 '
		      {: 
		         Eliom_duce.Xhtml.user_type_select
		         Vm.pair_to_string
                         ~a: {{ {class="version_select"} }}
		         ~name: diff1
		         (List.hd diff_list)
                         (List.tl diff_list)
                         :}
		      <br> []
		    'Version 2 '
		      {: 
		         Eliom_duce.Xhtml.user_type_select
		         Vm.pair_to_string
                         ~a: {{ {class="version_select"} }}
		         ~name: diff2
		         (List.hd diff_list)
                         (List.tl diff_list)
                         :}
		      {: Eliom_duce.Xhtml.button
		         ~button_type: {: "submit" :}
		       {{ "Execute diff" }}
		         :} 
		  ]] }}
	      in
              Lwt.return (({{ [ 
                              <tr class="sources_menu"> [
                                <td class="sources_menu_current">
                                  ['File options page']]
                              <tr class="sources_menu"> [
                                <td class="sources_menu_item">
			          [ {: Eliom_duce.Xhtml.get_form
                                       ~a: {{ {class="version_select"} }}
				       ~service: ps.Sh.sources_service
				       ~sp  
				       (file_version_select_form ~annotate:false) :} 
			          ]]
                               <tr class="sources_menu"> [
                                 <td class="sources_menu_item"> 
                                   [ {:Eliom_duce.Xhtml.get_form
                                        ~a: {{ {class="version_select"} }}
				        ~service: ps.Sh.sources_service
				        ~sp  
				        (file_version_select_form ~annotate:true)
                                        :}]]
                               !{:
                                  match diff_list with
                                  | [] -> {{ [] }}
                                  | _ -> 
                                      {{ [<tr class="sources_menu"> [
                                        <td class="sources_menu_item">
				          [ {: Eliom_duce.Xhtml.get_form
                                               ~a: {{ {class="version_select"} }}
				               ~service: ps.Sh.sources_service
				               ~sp  
				               file_diff_form :} 
				        ]]] }} :} ] }}, 
			  {{ [ <table class="log_table"> [!log_table_header !log]] }})
			   : ({{ [Xhtmltypes_duce.tr*] }}*{{ Xhtmltypes_duce.flows }})))
	    (fun exn ->
              let error_content = match exn with
              | Vm.Node_not_found -> error "Error: node not found"
              | Vm.Revision_not_found -> error "Error: revision not found"
              | _ -> error "Version manager internal error"
              in error_content >>= fun content -> Lwt.return ({{ [] }},content))
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
                              <span class="annot" style={:("display:block; background-color:"^color):}> {: utf8 :} 
                                !b] }}) 
                            : ({{ [Xhtmltypes_duce.span*] }}*{{ [Xhtmltypes_duce.span*] }})))
            (function _ ->
              Lwt.return (({{ [<span style={:("display:block; background-color:"^color):}> 
                                {: aut :}  !a]}},
                           {{ [
                              <span class="annot" style={:("display:block; background-color:"^color):}> {: line :} 
                                
                                !b] }}) 
                            : ({{ [Xhtmltypes_duce.span*] }}*{{ [Xhtmltypes_duce.span*] }})))
  in
  Data.get_area_for_task sp id >>= fun r_infos -> 
    match (r_infos.Types.r_repository_kind,r_infos.Types.r_repository_path) with
    | (Some(kind),Some(path)) ->
        Ocsforge_version_managers.get_fun_pack kind >>= fun fun_pack ->
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
                              <div class="annot_author"> [
                                <pre> [!a]
                              ] 
                              <div class="annot_content"> [
                                <pre> [<code>[!b]] 
                              ]
                            ] }})
            (function 
              | Vm.Node_not_found -> error "Error: node not found"
              | Vm.Revision_not_found -> error "Error: revision not found"
              | _ -> error "Version manager internal error")
    | _ -> failwith "Unable to retrieve repository informations"
	 


let draw_repository_table ~sp ~id ~version ~dir =
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let (kind,title,path,table) = match (version,dir) with
      | (None,None) -> (`Latest_version_list," : latest version",[],
                        create_repository_table_content ~sp ~id ~version ~dir:None
                          ~project_services:ps) 
      | (None,Some(rep)) -> 
          (`Latest_version_list," : latest version",rep,
           (create_repository_table_content 
              ~sp ~id ~version 
              ~dir ~project_services:ps))
      | (Some(v),None) -> (`Older_version_list,(" : version "^v),[],
                           create_repository_table_content ~sp ~id ~version 
                             ~dir:None
                             ~project_services:ps)
      | (Some(v),Some(rep)) -> 
          (`Older_version_list,(" : version "^v),rep,
           create_repository_table_content ~sp ~id ~version 
             ~dir ~project_services:ps)
      
      in 
      let (current_dir,current_dir_path) = build_path path in
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"" ~ps) in
      table >>= fun b ->
        let menu_content = {{[] }}
	in
        Lwt.return ({{  [<div class="path"> [ !a <span> {: current_dir :} 
                                                <span> {: title :}]
                         <br>[]
                         (sources_page_content sp kind menu_content b ps)
		               ] }} 
		      : {{ Xhtmltypes_duce.flows }})
          
              
let draw_source_code_view ~sp ~id ~target ~version =
  let file = Ocsigen_extensions.string_of_url_path ~encode:false target in
  let (current_dir,current_dir_path) = build_path target in
  let dir_version = match version with
    | None -> "head"
    | Some(v) -> v
  in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"File content - " ~ps) in
      create_source_code_content ~sp ~id ~file ~version >>= fun b ->
        let menu_content = 
          {{
           [<tr class="sources_menu"> [
             <td class="sources_menu_item">
	       [{:
		   Eliom_duce.Xhtml.a 
		   ~service:ps.Sh.sources_service
                   ~a: {{ {class="sources_menu_link"} }}
		   ~sp {{ {: "File options page" :}  }}
		   (target,(version,(true,(None,(false,(false,(None,None))))))):}]   
            ]
            <tr class="sources_menu"> [
              <td class="sources_menu_current"> {: "View content" :}]
            <tr class="sources_menu"> [
              <td class="sources_menu_item">
                [{:
                    Eliom_duce.Xhtml.a 
                    ~a: {{ {class="sources_menu_link"} }}
		    ~service:ps.Sh.sources_service
		    ~sp {{ {: "Annotate" :}  }}
		    (target,(version,(false,(None,(false,(true,(None,None))))))) :}]
            ]
           ] 
         }}
        in
        Lwt.return ({{ [  
                       <div class="path"> [ !a <span> {: current_dir :} 
                                                <span> {: (" @ "^dir_version) :}]
                           <br> []
		       	   {{ (sources_page_content ~kind:`Cat sp menu_content b ps) }}] }} 
		      : {{ Xhtmltypes_duce.flows }})
          

let draw_log_table ~sp ~id ~file ~start_rev ~end_rev = 
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      create_log_table_content 
        ~sp 
        ~id 
        ~file 
        ~range:(Some(start_rev,end_rev))
        ~project_services:ps >>= 
      fun b ->
        let menu_content = {{ [] }}
        in
        Lwt.return ({{ [
		       <div class="path"> ['Repository history']
			 <br> []
                           {{ (sources_page_content ~kind:`Log sp menu_content b ps) }}
                     ]}} : {{ Xhtmltypes_duce.flows }})
          

let draw_diff_view ~sp ~id ~target ~diff1 ~diff2 =
  let file = Ocsigen_extensions.string_of_url_path ~encode:false target in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a = (path_title ~sp ~path:current_dir_path ~version:None ~title:"File diff - " ~ps) in
      create_diff_view_content ~sp ~id ~file ~diff1 ~diff2 >>= fun b ->
        let menu_content = 
          {{
          [<tr class="sources_menu"> [
            <td class="sources_menu_item">
              [{:
		  Eliom_duce.Xhtml.a 
                  ~a: {{ {class="sources_menu_link"} }}
		  ~service:ps.Sh.sources_service
		  ~sp {{ {: "File options page" :}  }}
		  (target,(None,(true,(None,(false,(false,(None,None))))))):}]
          ]
          <tr class="sources_menu"> [
            <td class="sources_menu_item">
              [{:
		  Eliom_duce.Xhtml.a 
                  ~a: {{ {class="sources_menu_link"} }}
		  ~service:ps.Sh.sources_service
		  ~sp {{ {: "View content" :}  }}
		  (target,(None,(false,(None,(true,(false,(None,None))))))):}]
          ]
          <tr class="sources_menu"> [
            <td class="sources_menu_item">
              [{:
		  Eliom_duce.Xhtml.a 
                  ~a: {{ {class="sources_menu_link"} }}
		  ~service:ps.Sh.sources_service
		  ~sp {{ {: "Annotate" :}  }}
		  (target,(None,(false,(None,(false,(true,(None,None))))))):}]
          ]
          <tr class="sources_menu"> [
            <td class="sources_menu_current"> {: "File diff" :}]
          ] 
          }}
        in
        Lwt.return ({{ [   
                       <div class="path"> [ !a <span> {: current_dir :}]
                       <br> []
		       {{ (sources_page_content ~kind:`Diff sp menu_content b ps) }} ]}} 
                      : {{ Xhtmltypes_duce.flows }})
          

(* TODO ¿ cas ou target est un répertoire ? *)
let draw_file_page ~sp ~id ~target ~range =
  let rng = match range with
    | None -> (None,None)
    | Some(r) -> r
  in
  let (version,str_version) = match rng with
    | (None,_)  -> (None,"head") 
    | (Some(s),None) -> (Some(s),(" @ "^s))
    | (Some(s),Some(e)) -> (Some(s), (s^" -> "^e))
  in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"Browse file - " ~ps) in
      create_file_page ~sp ~id ~target ~range:rng ~project_services:ps >>= fun (menu_content,page_content) ->
        Lwt.return ({{ [ 
		       <div class="path"> [ !a <span> {: (current_dir^" @ "^str_version) :}]
                           <br> []
                           {{ (sources_page_content sp ~kind:`Options menu_content page_content ps) }}] }}
                      : {{ Xhtmltypes_duce.flows }})
          

let draw_annotate ~sp ~id ~target ~version =
  let file_version = match version with
    | None -> "head"
    | Some(v) -> v
  in
  let (current_dir,current_dir_path) = build_path target in
  match Sh.find_service id with
  | None -> failwith "Project services not found"
  | Some(ps) ->
      let a = (path_title ~sp ~path:current_dir_path ~version ~title:"Annotate - " ~ps) in
      create_annotate_page ~sp ~id ~target ~version ~project_services:ps >>= fun b ->
        let menu_content = 
          {{
          [<tr class="sources_menu"> [
            <td class="sources_menu_item">
              [{:
		  Eliom_duce.Xhtml.a 
                  ~a: {{ {class="sources_menu_link"} }}
		  ~service:ps.Sh.sources_service
		   ~sp {{ {: "File options page" :}  }}
		  (target,(version,(true,(None,(false,(false,(None,None))))))):}] 
          ]
          <tr class="sources_menu"> [
            <td class="sources_menu_item">
              [{:
                 Eliom_duce.Xhtml.a 
                  ~a: {{ {class="sources_menu_link"} }}
		  ~service:ps.Sh.sources_service
		   ~sp {{ {: "View content" :}  }}
		  (target,(version,(false,(None,(true,(false,(None,None))))))) :}]
          ]
          <tr class="sources_menu"> [
            <td class="sources_menu_current"> {: "Annotate" :}]
          ] }}
        in
        Lwt.return ({{ [
                       <div class="path"> [ !a <span> {: (current_dir^" @ "^file_version) :}]
                           <br> []
                       	   {{ (sources_page_content sp ~kind:`Annot menu_content b ps) }}]}} 
                      : {{ Xhtmltypes_duce.flows }})
