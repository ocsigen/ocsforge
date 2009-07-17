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

let add_message ~forum () = 
  Forum_sql.get_forum ~forum () >>= fun f ->
  let wiki = f.Forum_sql.Types.f_messages_wiki in
  Wiki_sql.get_wiki_info_by_id wiki >>= fun wiki_info ->
  let content_type = 
    Wiki_models.get_default_content_type wiki_info.Wiki_types.wiki_model
  in
  let title_syntax = f.Forum_sql.Types.f_title_syntax in
    Ocsforge_sql.first_message ~forum ~wiki ~creator:User.admin
      ~text:"ocsforge_first_task" ~title_syntax ~content_type

let first_task () =
  (*checking for already existing tasks*)
  Ocsforge_sql.get_task_count () >>= fun c ->
  if c > 0
  then Lwt.return ()
  else
    begin
      Forum_sql.get_forum ~title:"ocsforge_task_forum" () >>= fun fi ->
      Wiki_sql.new_wiki
        ~title:"ocsforge_task_wiki" ~descr:"" ~pages:(Some "ocsforge")
        ~boxrights:false ~staticdir:None ~author:User.admin
        ~model:Ocsisite.wikicreole_model ()
                                                          >>= fun (wiki,_) ->
      Wiki_sql.get_wiki_info_by_id wiki                   >>= fun wi ->
      Ocsforge_sql.new_area
        ~forum:(fi.Forum_sql.Types.f_id)
        ~wiki:(wi.Wiki_types.wiki_id)
        ()
                                                          >>= fun area ->
      add_message ~forum:(fi.Forum_sql.Types.f_id) ()     >>= fun message ->
      Ocsforge_sql.bootstrap_task
        ~area
        ~message                                          >>= fun task ->
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
        >>= fun _ ->
          Printf.printf
            "Your first ocsforge task as %ld id\nUse <<ocsforge_tree id=\"%ld\">> in the newly created wiki (path : 'ocsforge')\n%!"
            task task ;
          Lwt.return ()
    end

let _ = Lwt_unix.run ( first_task () )

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
    inline_widget
  
let _ = 
  Ocsforge_wikiext_source.register_wikiext 
    Wiki_syntax.wikicreole_parser

let _ = Printf.printf "done registering wikiext\n%!"


