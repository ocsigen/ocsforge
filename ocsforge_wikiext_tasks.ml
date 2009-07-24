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

let register_wikiext wp
      (tree_widget : Ocsforge_widgets_tasks.tree_widget)
      inline_widget
     =

  Wiki_syntax.add_extension
    ~wp ~name:"ocsforge_tree" ~wiki_content:false
    ((fun bi args content ->

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
            let () =  send_css_up "ocsforge_tree.css" sp
            in
            tree_widget#display ~sp ~root_task:id inline_widget
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

    )) ;


(*
  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_new_task" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let parent = (Types.task_of_string (List.assoc "parent" args)) in
            add_task_widget#display ~sp ?classes ~parent ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;

  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_task" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let id = Types.task_of_string (List.assoc "id" args) in
            Roles.get_area_role ~sp id >>= fun role ->
            task_widget#display ~sp ?classes id ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;


  (Wiki_syntax.add_extension ~wp ~name:"ocsforge_tasks" ~wiki_content:
     (fun bi args content ->

       Wikicreole.Block
         (let classes = 
            try Some [List.assoc "class" args]
            with Not_found -> None
          in
          try
            let sp = bi.Wiki_widgets_interface.bi_sp in
            let ids =
              List.map Types.right_area_of_string (Lang.assoc_all "id" args)
            in
            let params = List.map (Lang.assoc_all "param" args)
            in
            Roles.get_area_role ~sp id >>= fun role ->
            tree_widget#display ~sp ?classes ids params ()
            >>= fun (b : Xhtmltypes_duce.block) -> Lwt.return {{ [ {: b :} ] }}
          with Not_found | Failure _ -> 
            let s = Wiki_syntax.string_of_extension "raw" args content in
            Lwt.return {{ [ <b>{: s :} ] }}
         )

     )
  ) ;
 *)

