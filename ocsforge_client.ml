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
    form#add_common ( save_button      :> AXOWidgets.common ) ;

    popup#show





(*changing coloration depending on fields values*)
module Row_color =
struct
   
   let rec browse_table node func =
     match Js.Node.get_attribute node "tagName" with
       | "TABLE" | "TBODY" | "THEAD" -> let nodes =Js.Node.children node in
           List.iter (fun n -> browse_table n func) nodes
       | "TR" -> func node ;
       | _ -> ()
   ;;
   
   let scan_classes n =
     try
       let s = Js.Node.get_attribute n "className" in
       Scanf.bscanf (Scanf.Scanning.from_string s)
         "depth%d importance%d deadline%d complete%d"
          (fun _ i d c -> (i,d,c))
     with
         _ -> (0,0,0)
   
   
   let importance_highlighted = ref false
   let deadline_highlighted = ref false
   let complete_highlighted = ref false
   
   let color_fields field =
     browse_table (Js.get_element_by_id "ocsforge_tree")
       (match field with
         | "importance" ->
             let colors =
               if not !importance_highlighted
               then (fun n -> let (value, _, _) = scan_classes n in
                       ("background-color: rgb("
                          ^ (string_of_int 255) ^ ","
                          ^ (string_of_int (255 - (25 * value))) ^ ","
                          ^ (string_of_int (255 - (25 * value))) ^ ")")
                    )
               else (fun _ -> "background-color: rgb(255,255,255);")
             in
             let _ = importance_highlighted := not !importance_highlighted in
             let _ = deadline_highlighted := false in
             let _ = complete_highlighted := false in
               (fun n ->
                   Js.Node.set_attribute n "style"
                   (colors n) ;
                   () )
         | "deadline" ->
             let colors =
               if not !deadline_highlighted
               then (fun n -> let (_, value, _) = scan_classes n in
                       ("background-color: rgb("
                          ^ (string_of_int 255) ^ ","
                          ^ (string_of_int (255 - (25 * value))) ^ ","
                          ^ (string_of_int (255 - (25 * value))) ^ ")")
                    )
               else (fun _ -> "background-color: rgb(255,255,255);")
             in
             let _ = deadline_highlighted := not !deadline_highlighted in
             let _ = importance_highlighted := false in
             let _ = complete_highlighted := false in
               (fun n ->
                   Js.Node.set_attribute n "style"
                   (colors n) ;
                   () )
         | "complete" ->
             let colors =
               if not !complete_highlighted
               then (fun n -> let (_, _, value) = scan_classes n in
                       ("background-color: rgb("
                          ^ (string_of_int (255 - (25 * value))) ^ ","
                          ^ (string_of_int (255 - (25 * value))) ^ ","
                          ^ (string_of_int 255) ^ ")")
                    )
               else (fun _ -> "background-color: rgb(255,255,255);")
             in
             let _ = complete_highlighted := not !complete_highlighted in
             let _ = importance_highlighted := false in
             let _ = deadline_highlighted := false in
               (fun n ->
                   Js.Node.set_attribute n "style"
                   (colors n) ;
                   () )
   
         | _ -> (fun _ -> ())
       )
   ;;
end


let _ =
  let reg = Eliom_obrowser_client.register_closure in
  reg 189 new_task_pop_up ;
  reg 289 Row_color.color_fields ;


