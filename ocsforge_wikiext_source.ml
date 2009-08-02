(* Ocsimore
 * Copyright (C) 2009
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

open Ocsforge_wikiext_common

let wikicreole_parser = Wiki_syntax.wikicreole_parser
let reduced_wikicreole_parser0 = Wiki_syntax.reduced_wikicreole_parser0
let reduced_wikicreole_parser1 = Wiki_syntax.reduced_wikicreole_parser1
let reduced_wikicreole_parser2 = Wiki_syntax.reduced_wikicreole_parser2
let inline_wikicreole_parser = Wiki_syntax.inline_wikicreole_parser

let add_extension l ~name ~wiki_content f =
  List.iter (fun wp -> 
               Wiki_syntax.add_extension ~wp ~name ~wiki_content (f wp)) l

let register_wikiext wp = 
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0; reduced_wikicreole_parser1]
    ~name:"ocsforge_repository_tree" ~wiki_content:false
    ((fun _wp bi args content ->
      Wikicreole.Block
	(Lwt.catch
	   (fun () ->
	     let sp = bi.Wiki_widgets_interface.bi_sp in
       	     let id = List.assoc "id" args in
	     let file = 
	       try Some(List.assoc "file" args)
	       with Not_found -> None
	     in
	     let version = 
	       let v = List.assoc "version" args in
	       if (String.length v == 0) then
		 None
	       else Some(v)
	     in
             Ocsforge_wikiext_source.add_sources_css_header sp;
	     match file with
	       | None ->
		   Ocsforge_widgets_source.draw_repository_table ~sp ~id
		     ~version ~dir:None
		     >>=
		   fun (b: {{ Xhtmltypes_duce.flows }}) ->
		     Lwt.return b
	       | Some f ->
		   Ocsforge_widgets_source.draw_source_code_view ~sp ~id ~target:[f] ~version >>=
		   fun (b: {{ Xhtmltypes_duce.flows }}) ->
		     Lwt.return b
	   )
	   (function
	     | Not_found | Failure _ -> 
		 let s = Wiki_syntax.string_of_extension "raw" args content in
		 Lwt.return {{ [ <b>{: s :} ] }}
	     | exc ->
		 let s = Wiki_syntax.string_of_extension "raw" args content in
		 Lwt.return {{ [ <b>[ !{: s :} <br>[]
		 !{: Printexc.to_string exc :} ] ] }})
	)
     ))


let _ =
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0; reduced_wikicreole_parser1]
    ~name:"code"
    ~wiki_content:false
    (fun _wp bi args c ->
       Wikicreole.Block (
        let sp = bi.Wiki_widgets_interface.bi_sp in
        Ocsforge_widgets_source.add_sources_css_header sp;
         (match c with
           | None -> Lwt.return {{ [] }}
           | Some s -> 
             let lang = 
               try List.assoc "language" args
               with _ -> "" 
             in
             print_endline lang;
             let lexbuf = Lexing.from_string s in
             Ocsforge_color.color_by_lang lexbuf lang >>= fun (_, r) -> Lwt.return r
         ) >>= fun c ->
         Lwt.return {{ [ <pre class="color">{: c :} ] }})
    )

