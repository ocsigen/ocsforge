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

(** @author Raphael Proust *)

let (>>=) = Lwt.bind

let tree_css_header = Ocsimore_page.Header.create_header
  (fun sp ->
     {{ [ {:
         Eliom_duce.Xhtml.css_link
           ( Ocsimore_page.static_file_uri sp [ "ocsforge_tree.css" ] ) ()
     :} ] }})

let add_tree_css_header sp =
  Ocsimore_page.Header.require_header tree_css_header ~sp



let register_wikiext wp
      (tree_widget : Ocsforge_widgets_tasks.tree_widget)
      inline_widget
     =

  Wiki_syntax.add_extension
    ~wp ~name:"ocsforge_tree" ~wiki_content:false
    (fun bi args content ->

      Wikicreole.Block
        (Lwt.catch
           (fun () ->
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let id = Ocsforge_types.task_of_string (List.assoc "id" args) in
            let fields =
              let f = Ocsforge_lang.assoc_all "field" args in
                if f = []
                then ["importance"]
                else f
            in
            add_tree_css_header sp;
            tree_widget#display ~sp ~root_task:id
              >>= fun b ->
            Lwt.return ({{  b }} : {{ Xhtmltypes_duce.flows }})
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

    )



