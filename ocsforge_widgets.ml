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

let (>>=) = Lwt.bind
let (!!) = Lazy.force


let select_example_result = register_new_service
                              ~path:["select"]
                              ~get_params:(string "s")
                                       (fun sp g () ->
                                          return
                                            (html
                                               (head (title (pcdata "")) [])
                                               (body [p [pcdata "You selected: ";
                                                         strong [pcdata g]]])))

let create_select_form =
  (fun select_name ->
     [p [pcdata "Select something: ";
         Eliom_predefmod.Xhtml.string_select ~name:select_name
           (Eliom_predefmod.Xhtml.Option ([] (* attributes *),
                                          "Bob" (* value *),
                                          None (* Content, if different from value *),
                                          false (* not selected *))) (* first line *)
           [Eliom_predefmod.Xhtml.Option ([], "Marc", None, false);
            (Eliom_predefmod.Xhtml.Optgroup
               ([],
                "Girls",
                ([], "Karin", None, false),
                [([a_disabled `Disabled], "Juliette", None, false);
                 ([], "Alice", None, true);
                 ([], "Germaine", Some (pcdata "Bob's mother"), false)]))]
           ;
         Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Send" ()]])

let select_example = register_new_service ["select"] unit
                       (fun sp () () ->
                          let f =
                            Eliom_predefmod.Xhtml.get_form
                              select_example_result sp create_select_form
                          in
                            return
                              (html
                                 (head (title (pcdata "")) [])
                                 (body [f])))


class display_task_widget (*display (and propose edition for rightful users) a task*)
        (edit_task_service) =
object (self)

  (* val *_class = ??? *)

  method display
    ~sp ?task () =
    let rec draw_field_value ~title ~value ?alternatives ~string_of_value () =
      let title_ = title ^ " : " in
      match value with
        | None ->
            begin
              match alternatives with
                | None | Some [] -> {{ }}
                | Some [alt] ->
                    {{
                       title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], alt, None, false)) 
                         []
                         string_of_value :}
                    }}
                | Some alt_hd::alt_tl ->
                    {{
                      title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], alt_hd, None, false)) 
                         (List.map (fun a -> ([],a,None,false)) alt_tl)
                         string_of_value :}
                    }}
            end
        | Some value ->
            begin
              match alternatives with
                | None | Some [] -> {{ title_ value }}
                | Some [alt] ->
                    {{ title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], value, None, true)) 
                         [(Eliom_duce.Xhtml.Optgroup
                             ([], "Alternative",
                              ([], a, None, false), []))]
                         string_of_value :}
                    }}
                | Some alt_hd::alt_tl ->
                    {{ title_
                         {: Eliom_duce.Xhtml.user_type_select ~name:(*??*)
                         (Eliom_duce.Xhtml.Option ([], value, None, true))
                         [(Eliom_duce.Xhtml.Optgroup
                             ([], "Alternatives",
                              ([], alt_hd, None, false),
                              (List.map (fun a -> ([],a,None,false)) alt_tl)))]
                         string_of_value :}
                    }}
            end
    in
    let draw_rightfull_field ~right ~title ~value ~alternatives ~string_of_value () =
      if right
      then draw_field_value ~title ~value ~alternatives:(!!alternatives) ~string_of_value ()
      else draw_field_value ~title ~value ~string_of_value ()
    in
    Ocsforge_sql.get_task_by_id ~task_id:task >>= fun info ->
    Ocsforge.get_user_task_rights ~sp ~task_id:task >>= fun role ->
    !!(role.Ocsforge.task_reader) >>= b ->
    if not b then {{ 'Permission denied' }} else
    !!(role.Ocsforge.task_message_reader) >>= fun msg_reader ->
    !!(role.Ocsforge.task_property_editor) >>= fun prop_editor ->
      {{
        Forum_widget.display_message_widget#display_message ~sp ~message:info.Ocsforge_sql.Types.t_message
        {: if !!(role.Ocsforge.task_message_reader)
           then Forum_widget.display_message_widget#display_message ~sp ~message:info.Ocsforge_sql.Types.t_comments
        :}
        <p>[
          {: draw_rightfull_field ~right:prop_editor ~title:"kind" ~value:info.Ocsforge_sql.Types.kind ~alternatives:(lazy (Ocsforge_sql.get_kinds_by_area ~area_id:info.Ocsforge_sql.Types.t_area ())) (fun k -> k) ()
          :}
          {: draw_rightfull_field ~right:prop_editor ~title:"progress" ~value:info.Ocsforge_sql.Types.progress ~alternatives:(lazy (OcsforgeLang.interval_list ~min:0 ~max:100 ~bump:5)) Int32.to_string ()
          :}]
      }}
