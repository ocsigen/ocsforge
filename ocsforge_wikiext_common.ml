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

let (>>=) = Lwt.bind
module Params = Eliom_parameters
module Types = Ocsforge_types
module Services = Ocsforge_services_tasks

(**Functions to pass CSS information *)

(* Functions to add a CSS header for wiki, and to query whether
*    this header must be added *)
let add_wiki_css_header, must_add_wiki_css_header =
  let key = Polytables.make_key () in
    (fun sp -> Polytables.set ~table:(Eliom_sessions.get_request_cache sp)
                 ~key ~value:true),
    (fun sp ->
       try Polytables.get ~table:(Eliom_sessions.get_request_cache sp) ~key
       with Not_found -> false)

(* We define the function generating this header there *)
let send_css_up css_file sp =
  add_wiki_css_header sp ;
  Ocsimore_page.add_html_header_hook
    (fun sp ->
       if must_add_wiki_css_header sp then
         {{ [ {: Eliom_duce.Xhtml.css_link
            (Ocsimore_page.static_file_uri sp [ css_file ]) () :}
         ] }}
       else {{ [] }}
    )