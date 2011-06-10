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

(* TODO : use the real forum wiki_model ; adapt forum rights (non generic inclusions)*)




module Roles = Ocsforge_roles
module Types = Ocsforge_types
module Tree  = Ocsforge_lang.Tree
module Olang = Ocsforge_lang

module FTypes = Forum_types
module FRoles = Forum
(*Can't compile without *) open Sql

let (>>=) = Lwt.bind
let (!!) = Lazy.force
let ($) = User_sql.Types.apply_parameterized_group

let do_sql f = Lwt_pool.use Sql.pool f

(** Local functions *)



(** {2 Database access with verification of permissions} *)

(** {4 Task and Area creation } *)

let new_task
      ~parent ~subject ~text
      ?length ?progress ?importance ?kind
      () =
  User.get_user_id ()                                     >>= fun creator ->
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:parent) >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_creator)                             >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (fun db ->
         Ocsforge_sql.get_area_by_id ~area_id:area db       >>= fun ainfo ->
           Forum_data.new_message
             ~forum:ainfo.Types.r_forum
             ~creator_id:creator
             ~subject
             ~text
             ()                                           >>= fun message ->
           Ocsforge_sql.new_task
             ~parent
             ~message
             ~creator
             ~version:ainfo.Types.r_version
             ?length ?progress ?importance ?kind
             ~area
             ()
      )
  else Lwt.fail Ocsimore_common.Permission_denied

let new_project
      ~parent ~name
      ?length ?importance ?kind ?repository_kind ?repository_path
      ?wiki_container
      () =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:parent) >>= fun parent_area ->
  Roles.get_area_role parent_area                         >>= fun role ->
  !!(role.Roles.subarea_creator)                          >>= fun b ->
  if not b
  then
    Lwt.fail Ocsimore_common.Permission_denied
  else
    Sql.full_transaction_block
      (fun db ->

         (* get area code *)
         Ocsforge_sql.next_right_area_id db             >>= fun c ->

         (* create forum *)
         let title_syntax = Forum_site.title_syntax in
         Forum.create_forum (*TODO: use Forum_data.new_forum ? pros : cleaner ; cons : wikis must be generated manually *)
           ~wiki_model:Wiki_site.wikicreole_model (*FIXME: give the real wiki model *)
           ~title_syntax
           ~title:(Printf.sprintf "Ocsforge_%s_(area_%s)_forum"
                      name (Types.string_of_right_area c)
           )
           ~descr:("Messages about tasks in the area"
                   ^ (Types.string_of_right_area c))
           ()                                          >>= fun finfo ->

         let forum = finfo.FTypes.f_id in
         let mwiki = finfo.FTypes.f_messages_wiki in
         let cwiki = finfo.FTypes.f_comments_wiki in

         (* create wiki *)
         Ocsforge_sql.get_path_for_area ~area:parent_area db >>= fun ppath ->
         let ppath = Olang.unopt ppath in
         User.get_user_id ()                                 >>= fun author ->

         Wiki.create_wiki
            ~title:(Printf.sprintf "Ocsforge_%s_(area_%s)_wiki"
                      name (Types.string_of_right_area c)
            )
            ~descr:("Wiki for ocsforge area" ^ (Types.string_of_right_area c))
            ~path:( ppath @ [ name ] )
            ~author
            ?container_text:wiki_container
            ~model:Wiki_site.wikicreole_model (*TODO : give the real model *)
            ()
                                                           >>= fun wiki ->

         (* TODO : give the real model *)

         Wiki_data.new_wikitextbox

            ~rights:( Wiki_models.get_rights Wiki_site.wikicreole_model )
            ~content_type:(
              Wiki_models.get_default_content_type Wiki_site.wikicreole_model
            )
            ~wiki
            ~author
            ~comment:( "ocsforge subcontainer for " ^ name )
            ~content:"<<content>>"
            ()
            >>= fun wikibox ->

         (* create area *)
           Ocsforge_sql.new_area
             ~id:c ~forum ~wiki ~wikibox ?repository_kind ?repository_path ()
         >>= fun _ -> (* the result can't be anything but [c] *)

         Lwt_util.iter_serial (* syncing rights with the comment wiki *)
           (fun (ar,fr) ->
              User.add_to_group ~user:(ar $ c) ~group:(fr $ cwiki)
           )
           [
            (Roles.task_comment_sticky_setter, FRoles.message_sticky_makers     ) ;
            (Roles.task_comment_reader,        FRoles.moderated_message_readers ) ;
            (Roles.task_comment_moderator,     FRoles.message_moderators        ) ;
            (Roles.task_comment_deletor,       FRoles.message_deletors          ) ;
            (Roles.task_comment_deletor_if_author, FRoles.message_deletors      ) ;
            (Roles.task_comment_writer,            FRoles.message_creators      ) ;
            (Roles.task_comment_writer_not_moderated,
                                                 FRoles.message_creators_notmod ) ;
           ] >>= fun () ->

         Lwt_util.iter_serial (* syncing rights with the message wiki *)
           (fun (ar,fr) ->
              User.add_to_group ~user:(ar $ c) ~group:(fr $ mwiki)
           )
           [
            (Roles.task_message_editor_if_author,
                                            FRoles.message_modifiers_if_creator ) ;
            (Roles.task_message_editor,     FRoles.message_modifiers            ) ;
            (Roles.task_creator,            FRoles.message_creators             ) ;
            (*FIXME: needs subarea_creator to be included in forum_creators...
            * But there's a typing problem !*)
           ]
         >>= fun () ->
         User.add_to_group
           ~user:(Roles.task_admin $ c) ~group:(FRoles.forum_admin $ forum)
         >>= fun () ->


          (* create message *)
            Forum_data.new_message
              ~forum ~creator_id:author ~subject:name ~text:""
              () >>= fun message ->

          (* create task *)
            Ocsforge_sql.new_task
              ~parent ~message ~creator:author ~version:"0.0"
              ?length ?importance ?kind
              ~area:c ~area_root:true
              () >>= fun task ->

          (*propagating rights into the new area*)
            Lwt_util.iter_serial
              (fun a -> User.add_to_group
                          ~user:(a $ c) ~group:(a $ parent_area))
              [Roles.task_reader ;
               Roles.task_comment_reader ;
               Roles.task_comment_moderator ;
               Roles.task_comment_sticky_setter ;
               Roles.task_comment_deletor ;
               Roles.task_comment_writer ;
               Roles.task_comment_writer_not_moderated ;
               Roles.task_property_editor ;
               Roles.task_message_editor ;
               Roles.task_message_editor_if_author ;
               Roles.task_admin ;
               Roles.task_creator ;
               Roles.task_mover ;
               Roles.task_mover_from ;
               Roles.task_mover_to ;
               Roles.subarea_creator ;
               Roles.kinds_setter ;
               Roles.version_setter ;
               Roles.repository_setter ; ]
          >>= fun () -> Ocsforge_sql.set_root_task ~area_id:c ~task db
          >>= fun () -> Lwt.return task

      )


let get_task ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
  if b
  then do_sql (Ocsforge_sql.get_task_by_id ~task_id:task)
  else Lwt.fail Ocsimore_common.Permission_denied

let get_area_for_task ~task =
  do_sql(Ocsforge_sql.get_area_info_for_task ~task_id:task)

let get_area_for_page ~page =
  do_sql(Ocsforge_sql.get_area_info_for_page ~page_id:page)

let get_task_history ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
  if b
  then do_sql (Ocsforge_sql.get_task_history_by_id ~task_id:task)
  else Lwt.fail Ocsimore_common.Permission_denied

let get_area ~area =
  do_sql (Ocsforge_sql.get_area_by_id ~area_id:area)

let filter_aux_read t =
  Roles.get_area_role t.Types.t_area >>= fun role ->
  !!(role.Roles.task_reader)

let filter_aux_edit t =
  Roles.get_area_role t.Types.t_area >>= fun role ->
  !!(role.Roles.task_property_editor)

let filter_task_list_for_reading tl =
 Ocsimore_lib.lwt_filter filter_aux_read tl

let filter_task_list_for_editing tl =
  Ocsimore_lib.lwt_filter filter_aux_edit tl

let get_tree ~root ?with_deleted ?depth () = (*TODO: use depth for sql transaction, not only for result sorting (not that obvious) *)
  (* get as list *)
  do_sql (Ocsforge_sql.get_tasks_in_tree ~root ?with_deleted ())
  (* filter *)
  >>= filter_task_list_for_reading
  (* tree-fy *)
  >>= (function
    | []       -> Lwt.fail Tree.Empty_tree
    | hd :: tl ->
        let rec aux tree = function
          | []   -> Lwt.return tree
          | h::t ->
              aux (Tree.insert
                     (fun p _ -> h.Types.t_parent = p.Types.t_id)
                     tree
                     (Tree.node h []))
                  t
        in aux (Tree.node hd []) tl)
  (* apply depth limit (TODO: merge this and the tree-fication) *)
  >>= (fun t ->
    let rec aux tree = function
      | n when n <= 0 -> { tree with Tree.children = [] }
      | n -> { tree with Tree.children =
                 List.map
                   (fun t -> aux t (pred n))
                   (Tree.get_children tree)
             }
    in
      match depth with
        | None -> Lwt.return t
        | Some depth -> Lwt.return (aux t depth))
  (* change projects into leaves. *)
  >>= fun t ->
  Lwt.return
    { t with Tree.children =
        List.map
          (fun ({ Tree.content = c } as t) ->
             if c.Types.t_area_root
             then { t with Tree.children = [] }
             else t
          )
        t.Tree.children
    }

let get_sub_tasks ~parent =
  Sql.full_transaction_block (Ocsforge_sql.get_tasks_by_parent ~parent)
  >>= filter_task_list_for_reading


let get_tasks_edited_by ~editor =
  Sql.full_transaction_block (Ocsforge_sql.get_tasks_by_editor ~editor ())
  >>= filter_task_list_for_reading


let edit_task ~task
      ?length ?progress ?importance ?kind
      () =

  if    length     = None && progress = None
     && importance = None && kind     = None
  then Lwt.return ()
  else
    begin
      do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)  >>= fun area ->
      Roles.get_area_role area                               >>= fun role ->
      !!(role.Roles.task_property_editor)                    >>= fun b ->
      if not b
      then
        Lwt.fail Ocsimore_common.Permission_denied
      else
        Sql.full_transaction_block (
          (fun db ->
            let task_id = task in
            User.get_user_id () >>= fun author ->
            Ocsforge_sql.copy_in_history ~task_id db >>= fun () ->
            Ocsforge_sql.stamp_edition ~task_id ~author db >>= fun () ->
            (match length with
               | None -> Lwt.return ()
               | Some length ->
                   Ocsforge_sql.set_length ~task_id ~length db)
            >>= fun () ->
            (match progress with
               | None -> Lwt.return ()
               | Some progress ->
                   Ocsforge_sql.set_progress ~task_id ~progress db)
            >>= fun () ->
            (match importance with
               | None -> Lwt.return ()
               | Some importance ->
                   Ocsforge_sql.set_importance ~task_id ~importance db)
            >>= fun () ->
            (match kind with
               | None -> Lwt.return ()
               | Some kind -> Ocsforge_sql.set_kind ~task_id ~kind db)
          )
        )
    end

let move_task ~task ~parent = (* For this function to work, the "move forum message" part have to be implemented ! *)

  do_sql (Ocsforge_sql.is_area_root ~task) >>= fun area_root ->
  if not area_root

  then
    (* adapt to the new parent inheritance : for a task *)
    Sql.full_transaction_block
      (fun db ->
        (*checking rights*)
        Ocsforge_sql.get_area_for_task ~task_id:parent db >>= fun area_new ->
        Roles.get_area_role area_new                      >>= fun role_new ->
        !!(role_new.Roles.task_mover_to)                  >>= fun mover_to ->
        Ocsforge_sql.get_area_for_task ~task_id:task db   >>= fun area_old ->
        Roles.get_area_role area_old                      >>= fun role_old ->
        !!(role_old.Roles.task_mover_from)                >>= fun mover_from ->

           if mover_from && mover_to
           then
           (*executing*)
             User.get_user_id () >>= fun author ->

             Ocsforge_sql.copy_in_history ~task_id:task db         >>= fun () ->
             Ocsforge_sql.stamp_edition ~task_id:task ~author db   >>= fun () ->
             Ocsforge_sql.set_parent ~task_id:task ~parent db      >>= fun () ->
             Ocsforge_sql.set_area ~task_id:task ~area:area_new db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)
           else Lwt.fail Ocsimore_common.Permission_denied
         )

  else
    (* keep in the same area : for a project *)
    Sql.full_transaction_block
      (fun db ->
        (*checking rights*)
        Ocsforge_sql.get_area_for_task ~task_id:task db  >>= fun area ->
        Roles.get_area_role area                         >>= fun role ->
        !!(role.Roles.task_mover)                        >>= fun mover ->

           if mover
           then
           (*executing*)
             User.get_user_id () >>= fun author ->
             Ocsforge_sql.copy_in_history ~task_id:task db >>= fun () ->
             Ocsforge_sql.stamp_edition ~task_id:task ~author db >>= fun () ->

            (* move message *)
    (*FIXME*)Lwt.return ()
    (*FIXME: Ocsforge_sql.get_task_by_id ~db ~task_id:task () >>= fun tinfo ->
             Forum_data.move_message
              ~sp ~message:(tinfo.Types.t_message) ~forum ~creator_id:author ()*)
            >>= fun _ ->
             Ocsforge_sql.set_parent ~task_id:task ~parent db >>= fun () ->
             Ocsforge_sql.change_tree_marks ~task_id:task ~parent_id:parent db

           (*permission denied*)
           else Lwt.fail Ocsimore_common.Permission_denied
         )

let edit_area ~area ?repository_path ?repository_kind ?version () =
  if repository_path = None && repository_kind = None && version = None
  then Lwt.return ()
  else
    begin
      Roles.get_area_role area                               >>= fun role ->
      !!(role.Roles.repository_setter)                       >>= fun b ->
      if not b
      then
        Lwt.fail Ocsimore_common.Permission_denied
      else
        begin
          let f db =
            (match repository_path with
               | None -> Lwt.return ()
               | Some r -> Ocsforge_sql.set_repository_path
                             ~area_id:area ~repository_path:r db) >>= fun () ->
            (match repository_kind with
               | None -> Lwt.return ()
               | Some r -> Ocsforge_sql.set_repository_kind
                             ~area_id:area ~repository_kind:r db) >>= fun () ->
            (match version with
               | None -> Lwt.return ()
               | Some v -> Ocsforge_sql.set_version ~area_id:area ~version:v db)
          in Sql.full_transaction_block f
        end
    end


let make_project ~task ?repository_kind ?repository_path () =

  (* creating a new area and getting rights right *)
    Sql.full_transaction_block
      (fun db ->

         (* checking rights *)
         Ocsforge_sql.get_area_for_task ~task_id:task db   >>= fun area_old ->
         Roles.get_area_role area_old                      >>= fun role_old ->
         !!(role_old.Roles.subarea_creator)                >>= fun b ->
         if not b (*we only check subarea_creator as it's the "su" of an area*)
         then Lwt.fail Ocsimore_common.Permission_denied
         else

         begin

           (* get area code *)
           Ocsforge_sql.next_right_area_id db             >>= fun c ->

           let title_syntax = Forum_site.title_syntax in
           (* create forum *)
           Forum.create_forum (*TODO: use Forum_data.new_forum ? *)
              ~wiki_model:Wiki_site.wikicreole_model (*TODO : give the real wiki model*)
              ~title_syntax (*TODO:give the real title_syntax*)
              ~title:("Ocsforge area"^(Types.string_of_right_area c)^" forum")
              ~descr:("Messages about tasks in the area"
                      ^ (Types.string_of_right_area c))
              ()                                          >>= fun finfo ->

          let forum = finfo.FTypes.f_id in
(*          let mwiki = finfo.FTypes.f_messages_wiki in
          let cwiki = finfo.FTypes.f_comments_wiki in
 *)

          (* create wiki *)
            (* getting the path *)
          Ocsforge_sql.get_path_for_area ~area:area_old db   >>= fun ppath ->
          let ppath = Olang.unopt ppath in
            (* getting the rigths *)
          Ocsforge_sql.get_task_by_id ~task_id:task db        >>= fun ti ->
          Ocsforge_sql.get_area_by_id ~area_id:area_old db    >>= fun ai ->
          Forum_sql.get_forum ~forum:ai.Types.r_forum ()      >>= fun fi ->
          Wiki_sql.get_wiki_info_by_id fi.Forum_types.f_messages_wiki
                                                              >>= fun wi ->
          let rights = Wiki_models.get_rights wi.Wiki_types.wiki_model in
            (* getting the wikibox *)
          Forum_sql.get_message ~message_id:ti.Types.t_message ()
                                                          >>= fun mi ->
            (* eventually : getting the content *)
          Wiki_data.wikibox_content
            ~rights
            (mi.FTypes.m_wikibox)                         >>= fun (_,name,_) ->
          User.get_user_id ()                             >>= fun author ->

          let name = Olang.unopt name in
          Wiki.create_wiki
             ~title:("Ocsforge area" ^ (Types.string_of_right_area c) ^ " wiki")
             ~descr:("Wiki for ocsforge area" ^ (Types.string_of_right_area c))
             ~path:( ppath @ [ name ] )
             ~author
             (*TODO: use ~container_text*)
             ~model:Wiki_site.wikicreole_model (*TODO : give the real model *)
             ()
                                                           >>= fun wiki ->
               (* TODO : give the real model *)

          Wiki_data.new_wikitextbox

             ~rights:(Wiki_models.get_rights Wiki_site.wikicreole_model)
             ~content_type:
               (Wiki_models.get_default_content_type Wiki_site.wikicreole_model)
             ~wiki:wiki
             ~author
             ~comment:("ocsforge subcontainer for "^name)
             ~content:"<<content>>"
             ()
             >>= fun wikibox ->

          (* create area *)
            Ocsforge_sql.new_area
              ~id:c ~forum ~wiki ~wikibox ?repository_kind ?repository_path ()
          >>= fun _ -> (* the result can't be anything but [c] *)

(* link rights on area and rights on forum... Needs modifications on forum.ml
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ cwiki))
           [(task_comment_sticky_setter, FRoles.message_sticky_makers);
            (task_comment_reader, FRoles.message_readers);
            (task_comment_moderator, FRoles.message_moderators);
            (task_comment_deletor, FRoles.message_deletors);
            (task_comment_writer, FRoles.message_creator);
            (task_comment_writer_not_moderated, FRoles.message_creators_notmod);
           ]
         >>= fun () ->
         Lwt_util.iter_serial
           (fun (ar,fr) -> User.add_to_group ~user:(ar $ c) ~group:(fr $ mwiki))
           [(task_message_editor_if_author, FRoles.message_editor_if_creator);
            (task_message_editor, FRoles.message_editors);
            (task_creator, FRoles.message_creators);
           ]
         >>= fun () ->

         User.add_to_group ~user:(task_admin $ c) ~group:(forum_admin $ forum)
         >>= fun () ->

*)

          (* create message *)
            Forum_data.new_message
              ~forum ~creator_id:author ~subject:name ~text:"" ()
          >>= fun _ ->

          (*propagating rights into the new area*)
            Lwt_util.iter_serial
              (fun a -> User.add_to_group
                          ~user:(a $ c) ~group:(a $ area_old))
                          [Roles.task_reader ;
                           Roles.task_comment_reader ;
                           Roles.task_comment_moderator ;
                           Roles.task_comment_sticky_setter ;
                           Roles.task_comment_deletor ;
                           Roles.task_comment_writer ;
                           Roles.task_comment_writer_not_moderated ;
                           Roles.task_property_editor ;
                           Roles.task_message_editor ;
                           Roles.task_message_editor_if_author ;
                           Roles.task_admin ;
                           Roles.task_creator ;
                           Roles.task_mover ;
                           Roles.task_mover_from ;
                           Roles.task_mover_to ;
                           Roles.subarea_creator ;
                           Roles.kinds_setter ;
                           Roles.version_setter ;
                           Roles.repository_setter ; ]
          >>= fun () ->

          (*making changes to task and area*)
          Ocsforge_sql.adapt_to_project_spawn
            ~spawning:task
            ~new_area:c ~old_area:area_old
            db
          >>= fun () ->

          (* making changes to subarea's wiki *)
          Ocsforge_sql.get_tasks_in_tree ~root:task ~with_deleted:true () db
          >>= fun subtasks ->
          Lwt_util.iter_serial
            (fun { Types.t_id = t } ->
               Ocsforge_sql.get_area_info_for_task t db >>= fun a ->
               Wiki_sql.get_wiki_info_by_id a.Types.r_wiki >>= fun wi ->
               let path =
                 Olang.insert_after_segment
                   (Neturl.split_path
                      (Olang.unopt wi.Wiki_types.wiki_pages))
                   name
                   ppath
               in
               Wiki_data.update_wiki
                 ~rights
                 ~path:(Some (Neturl.join_path path))
                 a.Types.r_wiki
            )
            subtasks


          end)



(** Tampering with kinds *)

let get_kinds ~area =
  Sql.full_transaction_block
  (Ocsforge_sql.get_kinds_for_area ~area_id:area)

let add_kinds ~area ~kinds =
  Roles.get_area_role area     >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.add_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let del_kinds ~area ~kinds =
  Roles.get_area_role area     >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.del_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let set_kinds ~area ~kinds =
  Roles.get_area_role area     >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.set_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

let swap_kinds ~area ~kinds =
  Roles.get_area_role area     >>= fun role ->
  !!(role.Roles.kinds_setter)  >>= fun b ->
  if b
  then
    Sql.full_transaction_block
      (Ocsforge_sql.swap_kinds_for_area ~area_id:area ~kinds)
  else Lwt.fail Ocsimore_common.Permission_denied

(** For digging up the subject of a task('s forum message) *)
let find_subject ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
    if b
    then Ocsforge_sql.find_subject_content ~task
    else Lwt.fail Ocsimore_common.Permission_denied


(** Geting and setting separators. *)
let get_separators ~task =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:task)   >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_reader)                              >>= fun b ->
    if b
    then do_sql (Ocsforge_sql.get_separators ~root_task:task)
    else Lwt.fail Ocsimore_common.Permission_denied

let insert_separator ~after ~content =
  do_sql (Ocsforge_sql.get_area_for_task ~task_id:after)  >>= fun area ->
  Roles.get_area_role area                                >>= fun role ->
  !!(role.Roles.task_creator)                             >>= fun b ->
    if b
    then (
      Sql.full_transaction_block
        (fun db ->
           Ocsforge_sql.get_task_by_id ~task_id:after db
           >>= fun { Types.t_tree_max = after } ->
           Ocsforge_sql.new_separator ~after ~content db )
    )
    else Lwt.fail Ocsimore_common.Permission_denied

let set_separator_content ~separator ~content =
  do_sql (Ocsforge_sql.get_area_for_separator ~separator) >>= fun area ->
  Roles.get_area_role area.Types.r_id                     >>= fun role ->
  !!(role.Roles.task_property_editor)                     >>= fun b ->
    if b
    then do_sql (Ocsforge_sql.set_separator_content ~separator ~content)
    else Lwt.fail Ocsimore_common.Permission_denied

let move_separator ~separator ~after =
  do_sql (Ocsforge_sql.get_area_for_separator ~separator)
                                          >>= fun { Types.r_id = old_area } ->
  Roles.get_area_role old_area                          >>= fun oa_role ->
  do_sql (Ocsforge_sql.get_area_info_for_task ~task_id:after)
                                          >>= fun { Types.r_id = new_area } ->
  Roles.get_area_role new_area                                   >>= fun na_role ->
  !!( oa_role.Roles.task_mover_from )                            >>= fun b1 ->
  !!( na_role.Roles.task_mover_to )                              >>= fun b2 ->
    if b1 && b2
    then (
      Sql.full_transaction_block
        (fun db ->
           Ocsforge_sql.get_task_by_id ~task_id:after db
           >>= fun { Types.t_tree_max = after } ->
           Ocsforge_sql.move_separator ~separator ~after db )
    )
    else Lwt.fail Ocsimore_common.Permission_denied

