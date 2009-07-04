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

let register_wikiext wp = 
  Wiki_syntax.add_extension
    ~wp ~name:"ocsforge_repository_tree" ~wiki_content:false
    ((fun bi args content ->
      Wikicreole.Block
	(Lwt.catch
	   (fun () ->
	     let sp = bi.Wiki_widgets_interface.bi_sp in
       	     let id = Ocsforge_types.task_of_string (List.assoc "id" args) in
	     let version = List.assoc "version" args in
	     let () =  send_css_up "ocsforge_sources.css" sp in
	     Ocsforge_widgets_source.draw_repository_table ~sp ~id ~version >>= 
	     fun (b : {{ [ Xhtmltypes_duce.table ] }}) ->
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
     ) : Wiki_syntax.syntax_extension )
