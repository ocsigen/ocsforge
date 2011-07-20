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
let reduced_wikicreole_parser0 = Wiki_syntax.reduced_wikicreole_parser0
let reduced_wikicreole_parser1 = Wiki_syntax.reduced_wikicreole_parser1
let reduced_wikicreole_parser2 = Wiki_syntax.reduced_wikicreole_parser2
let phrasing_wikicreole_parser = Wiki_syntax.phrasing_wikicreole_parser

let add_extension l ~name ~wiki_content f =
  List.iter (fun wp ->
               Wiki_syntax.add_extension ~wp ~name ~wiki_content (f wp)) l

let register_wikiext _wp =
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0; reduced_wikicreole_parser1]
    ~name:"ocsforge_repository_tree" ~wiki_content:false
    ((fun _wp _ args content ->
      Wikicreole.Flow5
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
     ))


let _ =
  add_extension
    [wikicreole_parser; reduced_wikicreole_parser0; reduced_wikicreole_parser1]
    ~name:"code"
    ~wiki_content:false
    (fun _wp _ args c ->
       Wikicreole.Flow5 (
        Ocsforge_widgets_source.add_sources_css_header ();
        lwt c = match c with
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
	in
        Lwt.return [ pre ~a:[ a_class ["color"] ] c ] )
    )

