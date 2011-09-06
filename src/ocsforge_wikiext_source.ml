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

(** @author Granarolo Jean-Henri *)

open Eliom_pervasives
open HTML5.M

let wikicreole_parser = Wiki_syntax.wikicreole_parser
let wikicreole_parser_without_header_footer = Wiki_syntax.wikicreole_parser_without_header_footer
let reduced_wikicreole_parser0 = Wiki_syntax.reduced_wikicreole_parser0
let reduced_wikicreole_parser1 = Wiki_syntax.reduced_wikicreole_parser1
let reduced_wikicreole_parser2 = Wiki_syntax.reduced_wikicreole_parser2
let phrasing_wikicreole_parser = Wiki_syntax.phrasing_wikicreole_parser

let f_tree _ args content =
  `Flow5
    (Lwt.catch
       (fun () ->
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
          Ocsforge_widgets_source.add_sources_css_header ();
	  match file with
	  | None ->
	      Ocsforge_widgets_source.draw_repository_table ~id
		~version ~dir:None
	  | Some f ->
	      Ocsforge_widgets_source.draw_source_code_view ~id ~target:[f] ~version
       )
       (function
	  (*	     | Not_found | Failure _ ->
		     let s = Wiki_syntax.string_of_extension "raw" args content in
		     Lwt.return [ b [pcdata s ] ] *)
	| exc ->
	    let s = Wiki_syntax.string_of_extension "raw" args content in
	    Lwt.return [ b [pcdata s; br ();
			    pcdata (Printexc.to_string exc) ] ])
    )

let register_wikiext () =
  Wiki_syntax.register_interactive_simple_flow_extension ~name:"tree" f_tree


let code_content args c =
  Ocsforge_widgets_source.add_sources_css_header ();
  match c with
  | None -> Lwt.return []
  | Some s ->
      let s =
        if String.length s >= 1 && s.[0] = '\n' then
          String.sub s 1 (String.length s - 1)
        else
          s
      in
      let lang =
        try List.assoc "language" args
        with _ -> ""
      in
      let lexbuf = Lexing.from_string s in
      lwt (_, r) = Ocsforge_color.color_by_lang lexbuf lang in Lwt.return r

let f_code _ args c =
  `Flow5 (
    lwt c = code_content args c in
    Lwt.return [ pre ~a:[ a_class ["ocsforge_color"] ] c ] )

let f_code_inline _ args c =
  `Phrasing_without_interactive (
    lwt c = code_content args c in
    Lwt.return [ span ~a:[ a_class ["ocsforge_color"] ] c ] )

let _ =
  Wiki_syntax.register_simple_flow_extension ~name:"code" f_code;
  Wiki_syntax.register_simple_phrasing_extension ~name:"code-inline" f_code_inline


