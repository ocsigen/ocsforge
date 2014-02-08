(* Ocsimore
 * Copyright (C) 2005
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

open Eliom_lib
let (>>=) = Lwt.bind
let (>|=) = Lwt.(>|=)
module Types = Ocsforge_types
module Olang = Ocsforge_lang

(*Can't compile w/o *) open Ocsi_sql

let ocsforge_right_areas_id_seq = (<:sequence< serial "ocsforge_right_areas_id_seq" >>)

let ocsforge_right_areas = (<:table< ocsforge_right_areas (
  id integer NOT NULL,
  forum_id integer NOT NULL,
  version text NOT NULL,
  repository_kind text,
  repository_path text,
  wiki integer NOT NULL DEFAULT(1),
  wiki_container integer,
  root_task integer,
  sources_container integer NOT NULL
) >>)

let ocsforge_task_kinds = (<:table< ocsforge_task_kinds (
  right_area integer NOT NULL,
  kind text NOT NULL
) >>)

let ocsforge_tasks_id_seq = (<:sequence< serial "ocsforge_tasks_id_seq" >>)

let ocsforge_tasks = (<:table< ocsforge_tasks (
  id integer NOT NULL DEFAULT(nextval $ocsforge_tasks_id_seq$),
  parent integer NOT NULL,

  message integer NOT NULL,

  edit_author integer NOT NULL,
  edit_time timestamp NOT NULL DEFAULT(localtimestamp ()),
  edit_version text NOT NULL,

  length interval,
  progress integer,
  importance integer,
  kind text,

  area integer NOT NULL,

  tree_min integer NOT NULL DEFAULT(0),
  tree_max integer NOT NULL DEFAULT(1),

  deleted boolean NOT NULL DEFAULT(false),

  area_root boolean NOT NULL
) >>)

let ocsforge_tasks_history = (<:table< ocsforge_tasks_history (
  id integer NOT NULL,
  parent integer NOT NULL,

  edit_author integer NOT NULL,
  edit_time timestamp NOT NULL DEFAULT(localtimestamp ()),
  edit_version text NOT NULL,

  length interval,
  progress integer,
  importance integer,
  kind text,

  area integer NOT NULL,

  deleted boolean NOT NULL DEFAULT(false)
) >>)

let ocsforge_tasks_separators_id_seq = (<:sequence< serial "ocsforge_tasks_separators_id_seq" >>)

let ocsforge_tasks_separators = (<:table< ocsforge_tasks_separators (
  id integer NOT NULL DEFAULT(nextval $ocsforge_tasks_separators_id_seq$),
  after integer NOT NULL,
  content text NOT NULL
) >>)

let wikis = (<:table< wikis (
  id integer NOT NULL,
  title text NOT NULL,
  descr text NOT NULL,
  pages text,
  boxrights boolean NOT NULL,
  container integer,
  staticdir text,
  model text NOT NULL,
  siteid text,
  deleted boolean NOT NULL
) >>)

let wikiboxescontent = (<:table< wikiboxescontent (
  version integer NOT NULL,
  comment text NOT NULL,
  author integer NOT NULL,
  content text,
  datetime timestamp NOT NULL,
  content_type text NOT NULL,
  wikibox integer NOT NULL,
  ip text
) >>)

let forums_messages_id_seq = (<:sequence< serial "forums_messages_id_seq" >>)

let forums_messages = (<:table< forums_messages (
  id integer NOT NULL DEFAULT(nextval $forums_messages_id_seq$),
  creator_id integer NOT NULL,
  datetime timestamp NOT NULL DEFAULT(localtimestamp ()),
  parent_id integer,
  root_id integer NOT NULL,
  forum_id integer NOT NULL,
  subject integer,
  wikibox integer NOT NULL,
  moderated boolean NOT NULL DEFAULT(false),
  special_rights boolean NOT NULL DEFAULT(false),
  tree_min integer NOT NULL DEFAULT(1),
  tree_max integer NOT NULL DEFAULT(2)
) >>)

(** {2 Database statement and queries.} *)

(** {3 makers } *)

(** {5 make task } *)

let new_task
      ~parent ~message ~creator ~version
      ?length ?progress ?importance ?kind
      ~area ?(area_root = false)
      () =
  let parent_id   = Types.sql_of_task parent in
  let message_id  = Forum_types.sql_of_message message in
  let creator_id  = User_sql.Types.sql_from_userid creator in
  let area_id     = Types.sql_of_right_area area in
  let now         = CalendarLib.Calendar.now () in
    Ocsi_sql.full_transaction_block
      (fun db ->
        Lwt_Query.view_one db (<:view< {
          t.tree_min;
          t.tree_max;
        } | t in $ocsforge_tasks$;
          t.id = $int32:parent_id$ >>)
        >>= fun sql_data ->
        Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
          tree_min = t.tree_min + 2;
        } | t.tree_min >= $sql_data#tree_max$ >>)
        >>= fun () ->
        Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
          tree_max = t.tree_max + 2;
        } | t.tree_max >= $sql_data#tree_max$ >>)
        >>= fun () ->
        Lwt_Query.query db (<:insert< $ocsforge_tasks$ := {
          id = ocsforge_tasks?id;
          parent = $int32:parent_id$;
          message = $int32:message_id$;
          edit_author = $int32:creator_id$;
          edit_time = $timestamp:now$;
          edit_version = $string:version$;
          length = of_option $Option.map Sql.Value.interval length$;
          progress = of_option $Option.map Sql.Value.int32 progress$;
          importance = of_option $Option.map Sql.Value.int32 importance$;
          kind = of_option $Option.map Sql.Value.string kind$;
          area = $int32:area_id$;
          area_root = $bool:area_root$;
          tree_min = $sql_data#tree_max$;
          tree_max = $sql_data#tree_max$ + 1;
          deleted = ocsforge_tasks?deleted;
        } >>)
       >>= fun () -> Ocsi_sql.Lwt_PGOCaml.serial4 db "ocsforge_tasks_id_seq"
       >>= fun i -> Lwt.return (Types.task_of_sql i))


(** {5 make area} *)

let new_area ?id ~forum ?(version = "0.0") ?repository_kind ?repository_path
             ?wiki_container ~wiki ~wikibox () =
  let forum = Forum_types.sql_of_forum forum in
  let wiki_container = Olang.apply_on_opted
                         Wiki_types.sql_of_wikibox wiki_container
  in
  let wiki = Wiki_types.sql_of_wiki wiki in
  let wikibox = Wiki_types.sql_of_wikibox wikibox in
  Ocsi_sql.full_transaction_block
    (fun db ->
      (match id with
        | None ->
            Lwt_Query.value db
              (<:value< nextval $ocsforge_right_areas_id_seq$ >>)
        | Some id ->
            Lwt.return (Types.sql_of_right_area id)
      )
      >>= fun id ->
      Lwt_Query.query db (<:insert< $ocsforge_right_areas$ := {
        id = $int32:id$;
        forum_id = $int32:forum$;
        version = $string:version$;
        repository_kind = of_option $Option.map Sql.Value.string repository_kind$;
        repository_path = of_option $Option.map Sql.Value.string repository_path$;
        wiki_container = of_option $Option.map Sql.Value.int32 wiki_container$;
        wiki = $int32:wiki$;
        root_task = null;
        sources_container = $int32:wikibox$;
      } >>)
      >>= fun () ->
      Lwt.return id
    )
  >>= fun i ->
  Lwt.return (Types.right_area_of_sql i)

let get_path_for_area ~area =
  let area = Types.sql_of_right_area area in
    (fun db ->
      Lwt_Query.view_one db (<:view< {
        r.wiki;
      } | r in $ocsforge_right_areas$;
        r.id = $int32:area$ >>)
      >>= fun wikiid ->
      Lwt_Query.view_one db (<:view< {
        w.pages;
      } | w in $wikis$;
        w.id = $wikiid#wiki$ >>)
      >>= fun sql_data ->
      Olang.apply_on_opted_lwt Neturl.split_path sql_data#?pages)

let next_right_area_id db =
  Lwt_Query.value db
    (<:value< nextval $ocsforge_right_areas_id_seq$ >>)
  >>= fun c ->
  Lwt.return
    (Types.right_area_of_sql c)

let next_task_id db =
  Lwt_Query.value db
    (<:value< nextval $ocsforge_tasks_id_seq$ >>)
  >>= fun c ->
  Lwt.return
    (Types.task_of_sql c)

(** {3 getters : getting info } *)

(** {5 getters for tasks} *)

let get_task_by_id ~task_id ?(with_deleted = false) =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.view_opt db (<:view< {
      t.id;
      t.parent;
      t.message;
      t.edit_author;
      t.edit_time;
      t.edit_version;
      t.length;
      t.progress;
      t.importance;
      t.kind;
      t.area;
      t.tree_min;
      t.tree_max;
      t.deleted;
      t.area_root;
    } | t in $ocsforge_tasks$;
      t.id = $int32:task_id$;
      t.deleted = $bool:with_deleted$ || t.deleted = false >>)
    >>= function
      | Some r -> Lwt.return (Types.get_task_info r)
      | None -> Lwt.fail Not_found)

let get_task_history_by_id ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
      Lwt_Query.view db (<:view< {
        h.id;
        h.parent;
        h.edit_author;
        h.edit_time;
        h.edit_version;
        h.length;
        h.progress;
        h.importance;
        h.kind;
        h.area;
      } | h in $ocsforge_tasks_history$;
        h.id = $int32:task$ >>)
      >>= fun history ->
      get_task_by_id ~task_id db
      >>= fun current ->
      Lwt.return (current, List.map Types.get_task_history_info history))

let get_tasks_by_parent ~parent ?(with_deleted = false)=
  let parent = Types.sql_of_task parent in
    (fun db ->
      Lwt_Query.view db (<:view< {
        t.id;
        t.parent;
        t.message;
        t.edit_author;
        t.edit_time;
        t.edit_version;
        t.length;
        t.progress;
        t.importance;
        t.kind;
        t.area;
        t.tree_min;
        t.tree_max;
        t.deleted;
        t.area_root;
      } | t in $ocsforge_tasks$;
        t.parent = $int32:parent$;
        t.deleted = $bool:with_deleted$ || t.deleted = false >>)
      >>= fun r -> Lwt.return (List.map Types.get_task_info r))

let get_tasks_in_tree ~root ?(with_deleted = false) () db =
  let root = Types.sql_of_task root in
  Lwt_Query.view_one db (<:view< {
    t.tree_min;
    t.tree_max;
  } | t in $ocsforge_tasks$;
    t.id = $int32:root$ >>)
  >>= fun sql_data ->
  Lwt_Query.view db (<:view< {
    t.id;
    t.parent;
    t.message;
    t.edit_author;
    t.edit_time;
    t.edit_version;
    t.length;
    t.progress;
    t.importance;
    t.kind;
    t.area;
    t.tree_min;
    t.tree_max;
    t.deleted;
    t.area_root;
  } order by t.tree_min | t in $ocsforge_tasks$;
    t.tree_min >= $sql_data#tree_min$;
    t.tree_max <= $sql_data#tree_max$;
    t.deleted = $bool:with_deleted$ || t.deleted = false >>)
  >>= fun r -> Lwt.return (List.map Types.get_task_info r)

let get_tasks_by_editor ~editor ?(with_deleted = false) () =
  let editor = User_sql.Types.sql_from_userid editor in
  (fun db ->
    Lwt_Query.view db (<:view< {
      t.id;
      t.parent;
      t.message;
      t.edit_author;
      t.edit_time;
      t.edit_version;
      t.length;
      t.progress;
      t.importance;
      t.kind;
      t.area;
      t.tree_min;
      t.tree_max;
      t.deleted;
      t.area_root;
    } | t in $ocsforge_tasks$;
      t.edit_author = $int32:editor$;
      t.deleted = $bool:with_deleted$ || t.deleted = false >>)
    >>= fun r -> Lwt.return (List.map Types.get_task_info r))

(*TODO : more getter by attributes (version, time, progress...) use "comprehensions" *)

(** {5 getters for area} *)


let get_area_for_task ~task_id =
  let task = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.view_opt db (<:view< {
      t.area;
    } | t in $ocsforge_tasks$;
      t.id = $int32:task$ >>)
    >>= function
      | Some r -> Lwt.return (Types.right_area_of_sql r#!area)
      | None -> Lwt.fail Not_found
    )

let get_area_for_page ~page_id =
  (fun db ->
    Lwt_Query.view_opt db (<:view< {
      r.id;
    } | r in $ocsforge_right_areas$; w in $wikis$;
      r.wiki = w.id;
      w.pages = $string:page_id$;
      is_not_null r.root_task >>)
    >>= function
      | Some r -> Lwt.return (Types.right_area_of_sql r#!id)
      | None -> Lwt.fail Not_found
    )


let get_area_info_for_task ~task_id =
  let task = Types.sql_of_task task_id in
    (fun db ->
      Lwt_Query.view_one db (<:view< {
        r.id;
        r.forum_id;
        r.version;
        r.repository_kind;
        r.repository_path;
        r.root_task;
        r.wiki_container;
        r.wiki;
        r.sources_container;
      } | r in $ocsforge_right_areas$; t in $ocsforge_tasks$;
        r.id = t.area;
        t.id = $int32:task$ >>)
    >|= Types.get_right_area_info)

let get_area_info_for_page ~page_id =
  (fun db ->
    Lwt_Query.view_one db (<:view< {
      r.id;
      r.forum_id;
      r.version;
      r.repository_kind;
      r.repository_path;
      r.root_task;
      r.wiki_container;
      r.wiki;
      r.sources_container;
    } | r in $ocsforge_right_areas$; w in $wikis$;
      r.wiki = w.id;
      w.pages = $string:page_id$;
      is_not_null r.root_task >>)
    >|= Types.get_right_area_info)

let get_area_by_id ~area_id =
  let area = Types.sql_of_right_area area_id in
    (fun db ->
      Lwt_Query.view_opt db (<:view< {
        r.id;
        r.forum_id;
        r.version;
        r.repository_kind;
        r.repository_path;
        r.root_task;
        r.wiki_container;
        r.wiki;
        r.sources_container;
      } | r in $ocsforge_right_areas$;
        r.id = $int32:area$ >>)
      >>= function
        | Some r -> Lwt.return (Types.get_right_area_info r)
        | None -> Lwt.fail Not_found)


let get_area_version ~area_id =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.view_opt db (<:view< {
      r.version;
    } | r in $ocsforge_right_areas$;
      r.id = $int32:area$ >>)
    >>= function
      | Some r -> Lwt.return r#!version
      | None -> Lwt.fail Not_found
  )

(** {3 history management : to record changes } *)

let copy_in_history ~task_id db =
  let task_id = Types.sql_of_task task_id in
  Lwt_Query.view_one db (<:view< {
    t.id;
    t.parent;
    t.edit_author;
    t.edit_time;
    t.edit_version;
    t.length;
    t.progress;
    t.importance;
    t.kind;
    t.area;
  } | t in $ocsforge_tasks$;
    t.id = $int32:task_id$ >>)
  >>= fun sql_data ->
  Lwt_Query.query db (<:insert< $ocsforge_tasks_history$ := {
    id = $sql_data#id$;
    parent = $sql_data#parent$;
    edit_author = $sql_data#edit_author$;
    edit_time = $sql_data#edit_time$;
    edit_version = $sql_data#edit_version$;
    length = $sql_data#length$;
    progress = $sql_data#progress$;
    importance = $sql_data#importance$;
    kind = $sql_data#kind$;
    area = $sql_data#area$;
    deleted = ocsforge_tasks_history?deleted;
  } >>)

let stamp_edition ~task_id ~author db =
  let task = Types.sql_of_task task_id in
  let editor = User_sql.Types.sql_from_userid author in
  let now = CalendarLib.Calendar.now () in
  get_area_for_task ~task_id db >>= fun area_id ->
  get_area_version ~area_id db >>= fun ver ->
  Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
    edit_author = $int32:editor$;
    edit_time = $timestamp:now$;
    edit_version = $string:ver$;
  } | t.id = $int32:task$ >>)

(** {3 setters : to tamper recorded tuples } *)

(** {5 setters for tasks} *)

let set_length ~task_id ~length =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      length = of_option $Option.map Sql.Value.interval length$;
    } | t.id = $int32:task_id$ >>)
  )

let set_progress ~task_id ~progress =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      progress = of_option $Option.map Sql.Value.int32 progress$;
    } | t.id = $int32:task_id$ >>)
  )

let set_importance ~task_id ~importance =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      importance = of_option $Option.map Sql.Value.int32 importance$;
    } | t.id = $int32:task_id$ >>)
  )

let set_kind ~task_id ~kind =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      kind = of_option $Option.map Sql.Value.string kind$;
    } | t.id = $int32:task_id$ >>)
  )

let set_area ~task_id ~area =
  let task_id = Types.sql_of_task task_id in
  let area = Types.sql_of_right_area area in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      area = $int32:area$;
    } | t.id = $int32:task_id$ >>)
  )

let set_parent ~task_id ~parent =
  let task_id = Types.sql_of_task task_id in
  let parent = Types.sql_of_task parent in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      parent = $int32:parent$;
    } | t.id = $int32:task_id$ >>)
  )

let set_deleted ~task_id ~deleted =
  let task_id = Types.sql_of_task task_id in
  (fun db ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      deleted = $bool:deleted$;
    } | t.id = $int32:task_id$ >>)
  )


(*TODO : multiple field tamperers *)

(** {5 setters for right area} *)

let set_repository_kind ~area_id ~repository_kind =
  let area_id = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
      repository_kind = of_option $Option.map Sql.Value.string repository_kind$;
    } | r.id = $int32:area_id$ >>))

let set_repository_path ~area_id ~repository_path =
  let area_id = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
      repository_path = of_option $Option.map Sql.Value.string repository_path$;
    } | r.id = $int32:area_id$ >>))

let set_version ~area_id ~version =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
      version = $string:version$;
    } | r.id = $int32:area$ >>))

let set_root_task ~area_id ~task =
  let area = Types.sql_of_right_area area_id in
  let task = Types.sql_of_task task in
  (fun db ->
    Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
      root_task = $int32:task$;
    } | r.id = $int32:area$ >>))

(** {3 tree tamperer : change the atributtes of tasks in a whole (sub)tree } *)
(*TODO*)

let change_tree_marks ~task_id ~parent_id =
  let task     = Types.sql_of_task task_id in
  let parent   = Types.sql_of_task parent_id in
  (fun db ->
    Lwt_Query.view_one db (<:view< {
      t.tree_min;
      t.tree_max;
    } | t in $ocsforge_tasks$;
      t.id = $int32:task$ >>)
    >>= fun sql_data ->
    Lwt_Query.view_one db (<:view< {
      t.tree_max;
    } | t in $ocsforge_tasks$;
      t.id = $int32:parent$ >>)
    >>= fun sql_data' ->
    let size = Int32.sub sql_data#!tree_max sql_data#!tree_min in
    let dist = Int32.sub sql_data'#!tree_max sql_data#!tree_max in
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      tree_min = t.tree_min + $int32:dist$;
      tree_max = t.tree_max + $int32:dist$;
    } | t.tree_min >= $sql_data#tree_min$;
      t.tree_max <= $sql_data#tree_max$ >>)
    >>= fun () ->
    Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
      tree_min = t.tree_min - $int32:size$;
      tree_max = t.tree_max - $int32:size$;
    } | t.tree_min > $sql_data#tree_max$;
      t.tree_max < $sql_data'#tree_max$ >>)
  )


(** {3 Managing Kinds} *)

let get_kinds_for_area ~area_id =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.view db (<:view< {
      t.kind;
    } | t in $ocsforge_task_kinds$;
      t.right_area = $int32:area$ >>)
    >>= Lwt_list.map_p (fun x -> Lwt.return x#!kind)
  )

let add_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     let f k =
       Lwt_Query.query db (<:insert< $ocsforge_task_kinds$ := {
         right_area = $int32:area$;
         kind = $string:k$;
       } >>)
     in Lwt_util.iter_serial f kinds)



let del_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
     let f (k,alt) =
       (match alt with
         | None -> Lwt.return ()
         | Some a ->
             Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
               kind = $string:a$
             } | t.kind = $string:k$;
               t.area = $int32:area$ >>))
       >>= fun () ->
       Lwt_Query.query db (<:delete< t in $ocsforge_task_kinds$
         | t.kind = $string:k$;
         t.right_area = $int32:area$ >>)
     in Lwt_util.iter_serial f kinds)


let set_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    Lwt_Query.query db (<:delete< t in $ocsforge_task_kinds$
      | t.right_area = $int32:area$ >>)
    >>= fun () ->
    add_kinds_for_area ~area_id ~kinds db)

let swap_kinds_for_area ~area_id ~kinds =
  let area = Types.sql_of_right_area area_id in
  (fun db ->
    let f (old,nu) =
      Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
        kind = $string:nu$;
      } | t.kind = $string:old$;
        t.area = $int32:area$ >>)
    in Lwt_util.iter_serial f kinds)


(** {3 managing separators} *)
let get_separators ~root_task =
  let task = Types.sql_of_task root_task in
  (fun db ->
    Lwt_Query.view_one db (<:view< {
      t.tree_min;
      t.tree_max;
    } | t in $ocsforge_tasks$;
      t.id = $int32:task$ >>)
    >>= fun sql_data ->
    Lwt_Query.view db (<:view< {
      s.id;
      s.after;
      s.content;
    } | s in $ocsforge_tasks_separators$; t in $ocsforge_tasks$;
      s.after = t.id;
      t.tree_min >= $sql_data#tree_min$;
      t.tree_max <= $sql_data#tree_max$ >>)
    >>= fun r -> Lwt.return (List.map Types.get_separator_info r))
let new_separator ~after ~content =
    (fun db ->
      Lwt_Query.query db (<:insert< $ocsforge_tasks_separators$ := {
        id = ocsforge_tasks_separators?id;
        after = $int32:after$;
        content = $string:content$;
      } >>)
    )
let set_separator_content ~separator ~content =
  let separator = Types.sql_of_separator separator in
    (fun db ->
      Lwt_Query.query db (<:update< s in $ocsforge_tasks_separators$ := {
        content = $string:content$;
      } | s.id = $int32:separator$ >>)
    )
let move_separator ~separator ~after =
  let separator = Types.sql_of_separator separator in
    (fun db ->
      Lwt_Query.query db (<:update< s in $ocsforge_tasks_separators$ := {
        after = $int32:after$;
      } | s.id = $int32:separator$ >>)
    )
let get_task_for_separator ~separator =
  let separator = Types.sql_of_separator separator in
  (fun db ->
    Lwt_Query.view_one db (<:view< {
      t.id;
      t.parent;
      t.message;
      t.edit_author;
      t.edit_time;
      t.edit_version;
      t.length;
      t.progress;
      t.importance;
      t.kind;
      t.area;
      t.tree_min;
      t.tree_max;
      t.deleted;
      t.area_root;
    } | t in $ocsforge_tasks$;
      t.tree_max = $int32:separator$ >>)
    >|= Types.get_task_info)
let get_area_for_separator ~separator =
  let separator = Types.sql_of_separator separator in
  (fun db ->
    Lwt_Query.view_opt db (<:view< {
      r.id;
      r.forum_id;
      r.version;
      r.repository_kind;
      r.repository_path;
      r.root_task;
      r.wiki_container;
      r.wiki;
      r.sources_container;
    } | r in $ocsforge_right_areas$; t in $ocsforge_tasks$; s in $ocsforge_tasks_separators$;
      r.id = t.area;
      t.tree_max = s.after;
      s.id = $int32:separator$ >>)
    >>= function
      | None -> Lwt.fail Not_found
      | Some r -> Lwt.return (Types.get_right_area_info r))


let adapt_to_project_spawn ~spawning ~new_area ~old_area =
  let old_area = Types.sql_of_right_area old_area in
  let new_area = Types.sql_of_right_area new_area in
  let spawn    = Types.sql_of_task spawning in
  let t_default = true in
    (fun db ->
      Lwt_Query.query db (<:update< t in $ocsforge_tasks$ := {
        area_root = $bool:t_default$;
        area = $int32:new_area$;
      } | t.id = $int32:spawn$ >>)
      >>= fun () ->
      Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
        root_task = $int32:spawn$;
      } | r.id = $int32:old_area$ >>)
    )


(**/**)

let find_subject_content ~task =
  let task = Types.sql_of_task task in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view_one db (<:view< {
        w.content;
      } | w in $wikiboxescontent$; f in $forums_messages$; t in $ocsforge_tasks$;
        nullable w.wikibox = f.subject;
        f.id = t.message;
        t.id = $int32:task$ >>)
      >>= fun s -> Lwt.return (Ocsforge_lang.unopt ~default:"" s#?content)
    )


let bootstrap_task ~area ~message =
  Ocsi_sql.full_transaction_block
    (fun db ->
       next_task_id db >>= fun id_ ->
       let id      = Types.sql_of_task id_ in
       let message = Forum_types.sql_of_message message in
       let area    = Types.sql_of_right_area area in
       let version = "0.0" in
       let kind    = "" in
       let now     = CalendarLib.Calendar.now () in
       let author  = User_sql.Types.sql_from_userid User.admin in
       let tmin    = Int32.zero in
       let tmax    = Int32.one in
       let root    = true in
       Lwt_Query.query db (<:insert< $ocsforge_tasks$ := {
         id = $int32:id$;
         parent = $int32:id$;
         message = $int32:message$;
         edit_author = $int32:author$;
         edit_time = $timestamp:now$;
         edit_version = $string:version$;
         kind = $string:kind$;
         area = $int32:area$;
         tree_min = $int32:tmin$;
         tree_max = $int32:tmax$;
         area_root = $bool:root$;
         deleted = ocsforge_tasks?deleted;
         importance = null;
         length = null;
         progress = null;
       } >>)
       >>= fun () ->
       Lwt_Query.query db (<:update< r in $ocsforge_right_areas$ := {
         root_task = $int32:id$;
       } | r.id = $int32:area$ >>)
       >>= fun () ->
       Lwt.return id_)

let get_task_count () =
  (*TODO: when tree management is bugproof, use tree_max & tree_min *)
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view_one db (<:view< group {
        nb_id = count[t.id];
      } | t in $ocsforge_tasks$ >>)
    )
  >>= fun i ->
  Lwt.return (Int64.to_int i#!nb_id)

let get_root_task () =
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view_opt db (<:view< {
        t.id;
        t.parent;
        t.message;
        t.edit_author;
        t.edit_time;
        t.edit_version;
        t.length;
        t.progress;
        t.importance;
        t.kind;
        t.area;
        t.tree_min;
        t.tree_max;
        t.deleted;
        t.area_root;
      } | t in $ocsforge_tasks$;
        t.tree_min = 0 >>)
    )
  >>= function
    | Some r -> Lwt.return (Types.get_task_info r)
    | None -> Lwt.fail Not_found


let is_area_root ~task =
  let task = Types.sql_of_task task in
  (fun db ->
    Lwt_Query.view_one db (<:view< {
      t.area_root;
    } | t in $ocsforge_tasks$;
      t.id = $int32:task$ >>)
    >>= fun x ->
    Lwt.return x#!area_root
  )

let get_projects_path_list () =
  Ocsi_sql.full_transaction_block
  (fun db ->
    Lwt_Query.view db (<:view< {
      w.pages;
    } | r in $ocsforge_right_areas$; w in $wikis$;
      r.wiki = w.id;
      is_not_null r.root_task >>)
  )
  >>= fun l -> Lwt.return ( Olang.filter_map (fun x -> x#?pages) l )

let get_project_path ~area () =
  let area = Types.sql_of_right_area area in
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view_one db (<:view< {
        w.pages;
      } | r in $ocsforge_right_areas$; w in $wikis$;
        r.wiki = w.id;
        is_not_null r.root_task;
        r.id = $int32:area$ >>)
      >>= fun x ->
      Lwt.return x#?pages
    )


let first_message ~forum ~wiki ~creator ~title_syntax ~text ~content_type =
  let moderated = false in
  let creator_id' = User_sql.Types.sql_from_userid creator in
  let forum_id = Forum_types.sql_of_forum forum in
  Ocsi_sql.full_transaction_block
    (fun db ->

       (*setting the wikibox for the core of the message*)
       Wiki_sql.new_wikibox
         ~db ~wiki ~author:creator ~comment:"" ~content:""
        ~content_type ()                >>= fun wikibox ->

       (*setting the wikibox for the subject*)
       Wiki_sql.new_wikibox
          ~db ~wiki ~author:creator ~comment:"" ~content:text
          ~content_type:title_syntax () >>= fun subject ->
       Lwt.return (Wiki_types.sql_of_wikibox subject)
                                        >>= fun subject ->

       (*putting data into the database*)
       let wikibox = Wiki_types.sql_of_wikibox wikibox in
       (Lwt_Query.value db (<:value< nextval $forums_messages_id_seq$ >>)
        >>= fun next_id ->
        Lwt_Query.query db (<:insert< $forums_messages$ := {
          id = $int32:next_id$;
          creator_id = $int32:creator_id'$;
          root_id = $int32:next_id$;
          forum_id = $int32:forum_id$;
          subject = $int32:subject$;
          wikibox = $int32:wikibox$;
          moderated = $bool:moderated$;
          datetime = forums_messages?datetime;
          parent_id = null;
          special_rights = forums_messages?special_rights;
          tree_min = forums_messages?tree_min;
          tree_max = forums_messages?tree_max;
        } >>)
       ) >>= fun () ->
      Ocsi_sql.Lwt_PGOCaml.serial4 db "forums_messages_id_seq" >>= fun s ->
      Lwt.return (Forum_types.message_of_sql s)
    )

let get_right_area_ids () =
  Ocsi_sql.full_transaction_block
    (fun db ->
      Lwt_Query.view db (<:view< {
        r.id;
        title = null;
      } | r in $ocsforge_right_areas$ >>)
    )
