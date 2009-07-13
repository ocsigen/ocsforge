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

(* This module is for boot straping (creating the first task, registering
 * services, etc.)*)

let (>>=) = Lwt.bind
let ($) = User_sql.Types.apply_parameterized_group

(*TODO : This is the biggest hack so far... It HAS to be changed !!!*)


let first_task () =
  (*checking for already existing tasks*)
  Lwt.return (Printf.printf "..!\n%!") >>= fun () ->
  Ocsforge_sql.get_task_count () >>= fun c ->
  Lwt.return (Printf.printf "..!\n%!") >>= fun () ->
  if c > 0
  then Lwt.fail (Failure "Can't bootstrap twice")
  else
    begin
      Lwt.return (Printf.printf "..!\n%!") >>= fun () ->
      Ocsforge_sql.new_area ~forum:(Forum_sql.Types.forum_of_sql (Int32.of_int 2)) ~wiki:(Wiki_types.wiki_of_sql Int32.one) ()
                                                       >>= fun area ->
      Ocsforge_sql.bootstrap_task ~area ~message:(Forum_sql.Types.message_of_sql Int32.one)
                                                       >>= fun _ ->
      Lwt_util.iter_serial
        (fun a -> User.add_to_group
                    ~user:(User_sql.Types.basic_user User.admin)
                    ~group:(a $ area))
        [Ocsforge_roles.task_reader ;
         Ocsforge_roles.task_comment_reader ;
         Ocsforge_roles.task_comment_moderator ;
         Ocsforge_roles.task_comment_sticky_setter ;
         Ocsforge_roles.task_comment_deletor ;
         Ocsforge_roles.task_comment_writer ;
         Ocsforge_roles.task_comment_writer_not_moderated ;
         Ocsforge_roles.task_property_editor ;
         Ocsforge_roles.task_message_editor ;
         Ocsforge_roles.task_message_editor_if_author ;
         Ocsforge_roles.task_admin ;
         Ocsforge_roles.task_creator ;
         Ocsforge_roles.task_mover ;
         Ocsforge_roles.task_mover_from ;
         Ocsforge_roles.task_mover_to ;
         Ocsforge_roles.subarea_creator ;
         Ocsforge_roles.kinds_setter ;
         Ocsforge_roles.version_setter ; ]
     >>= fun () ->
       Lwt.return (Printf.printf "bootstraping done !\n%!")
    end

(* THIS HAS BEEN DONE !
let _ =
  Lwt_unix.run(
    Lwt.return (Printf.printf "bootstraping tasks !\n%!") >>= fun () ->
    first_task ())
 *)


let forge_wiki_model = Ocsisite.wikicreole_model (*TODO: use a real wiki model*)

let _ = Printf.printf "registering wikiext\n%!"

let _ =
(*let wiki_widgets = Wiki_models.get_widgets forge_wiki_model in            *)
(*let services = Forum_services.register_services () in                     *)
(*let widget_err = new Widget.widget_with_error_box in                      *)
(*let add_message_widget = new Forum_widgets.add_message_widget services in *)
  let tree_widget = new Ocsforge_widgets_tasks.tree_widget in
(*let new_task_widget = new Ocsforge_widgets.new_task_widget in*)
  let error_box = new Widget.widget_with_error_box in
  let inline_widget = new Wiki_widgets.frozen_wikibox error_box in
  
  Ocsforge_wikiext_tasks.register_wikiext
    Wiki_syntax.wikicreole_parser
    tree_widget
    Ocsforge_services_tasks.non_localized_service
    Ocsforge_services_source.temp_service
    inline_widget
  
let _ = 
  Ocsforge_wikiext_source.register_wikiext 
    Wiki_syntax.wikicreole_parser

let _ = Printf.printf "done registering wikiext\n%!"


