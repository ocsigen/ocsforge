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

(** The following registering function is never called. Uncomment the
  * corresponding line in ocsforge_bootstrap.ml to make this aviable. *)


let register_wikiext wp (tree_widget : Ocsforge_widgets_tasks.tree_widget) =
  (* register the <<ocsforge_tree id="42">> syntax extension *)

  Wiki_syntax.add_extension ~wp ~name:"ocsforge_tree" ~wiki_content:false
    (fun bi args content ->

      Wikicreole.Block
        (Lwt.catch
           (fun () ->
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let id = Ocsforge_types.task_of_string (List.assoc "id" args) in

            ( tree_widget#display ~sp ~root_task:id )
           )

           (function
              | Not_found | Failure _ ->
                  let s = Wiki_syntax.string_of_extension "raw" args content in
                  Lwt.return [ b [pcdata s ] ]
              | exc ->
                  let s = Wiki_syntax.string_of_extension "raw" args content in
                  Lwt.return [ b [ pcdata s; br ()
                    pcdata (Printexc.to_string exc) ] ] )
        )
    )



