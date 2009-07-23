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

open AXOLang

(*poping up the new task form*)
let new_task_pop_up id =

  let form = new AXOToolkit.block_container in
  let popup = new AXOToolkit.popup form in

  let title_input = new AXOToolkit.text_input "Enter your title here" in
    title_input#set_attribute "size" "100" ;
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_task") ;
                ("parent", Int32.to_string id) ;
                ("subject", title_input#get_value) ;
                ("text", "") ;
                ("length", "") ;
                ("progress", "") ;
                ("importance", "") ;
                ("deadline_t", "") ;
                ("deadline_v", "") ;
                ("kind", "") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "task successfully added")
             (c,m) ;
      ) ;

    form#add_common ( title_input      :> AXOWidgets.common ) ;
    form#add_common ( new AXOToolkit.br ) ;
    form#add_common ( save_button      :> AXOWidgets.common ) ;

    popup#show

let edit_task_popup
      id subject length progress importance deadline_t deadline_v kind
      =
  let form = new AXOToolkit.block_container in
  let popup = new AXOToolkit.popup form in

  let title_input = new AXOToolkit.text_input subject in
    title_input#set_attribute "size" "100" ;
  let progress_input = (*TODO : switch to in32 based progress*)
    new AXOToolkit.select
      (LOption.string_of_t_opt string_of_int)
      (LOption.t_opt_of_string int_of_string)
      progress
      (LList.t_opt_list_of_t_list
         (LList.int_interval_list ~bump:5 ~min:0 ~max:100 ()))
  in
  let save_button = new AXOToolkit.inline_text_widget_button "SAVE" in
    save_button#add_click_action
      (fun () ->
         popup#hide ;
         let (c,m) =
           AXOCom.http_post "./"
              [ ("__eliom_na__name","ocsforge_add_task") ;
                ("parent", Int32.to_string id) ;
                ("subject", title_input#get_value) ;
                ("text", "") ;
                ("length", "") ;
                ("progress", LOption.string_of_t_opt string_of_int progress_input#get_value) ;
                ("importance", "") ;
                ("deadline_t", "") ;
                ("deadline_v", "") ;
                ("kind", "") ;
              ]
         in
           AXOCom.alert_on_code
             ~on_2xx:(fun _ -> AXOJs.alert "task successfully edited")
             (c,m) ;
      ) ;

    List.iter (fun c -> form#add_common c)
      [
        (                                title_input :> AXOWidgets.common ) ;
        (                          new AXOToolkit.br                      ) ;
        ( (new AXOToolkit.inline_text "progress : ") :> AXOWidgets.common ) ;
        (                          new AXOToolkit.br                      ) ;
        (                                save_button :> AXOWidgets.common ) ;
      ]





