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

module Encode =
struct

  let seek_and_destroy seek destroy str =
    let rec aux str =
      try
        let i = String.index str seek in
            (String.sub str 0 i)
          ^ destroy
          ^ (aux (String.sub str (succ i) ((String.length str) - (succ i))))
      with Not_found -> str
    in aux str

  let percent_assoc = (*/!\ '%' must be first ; ' ' must be last !*)
    [('%', "%25") ; ('!', "%21") ; ('*', "%2A") ; ('"', "%22") ; ('\'', "%27");
     ('(', "%28") ; (')', "%29") ; (';', "%3B") ; (':', "%3A") ; ('@', "%40") ;
     ('&', "%26") ; ('=', "%3D") ; ('+', "%2B") ; ('$', "%24") ; (',', "%2C") ;
     ('/', "%2F") ; ('?', "%3F") ; ('#', "%23") ; ('[', "%5B") ; (']', "%5D") ;
     (' ', "+")   ]

  let urlencode_string str =
    List.fold_left (fun a (s,d) -> seek_and_destroy s d a)
      str percent_assoc
     
  let urlencode args =
   String.concat "&"
     (List.map
        (fun (n,v) -> (urlencode_string n) ^ "=" ^ (urlencode_string v))
        args
     )

end



(*poping up the new task form*)
let pop_up_new_task id =
  let body = Js.get_element_by_id "ocsforge_tree" in
  let mask =
    Js.Html.div
      ~style:"position: fixed; right: 0px; top: 0px; width: 100%; \
              height: 100%; background-color: black; opacity: .5;"
      []
  in
  let savable = ref false in
  let title_input =
    Js.Html.input
      (fun s -> s)                                (* format... (string_of_t)  *)
      (fun s -> s)                                (* parse...  (t_of_string)  *)
      ""                                          (* value...                 *)
      100                                         (* size...                  *)
      true                                        (* editable...              *)
      (fun s ->
         if s.Js.Html.get () = ""
         then savable := false
         else savable := true  )                  (* callback...              *) (*TODO: use the call back to grey out the "SAVE" button*)
  in
(* let progress_input =
 *   Js.Html.input
 *     (function
 *        | None -> ""
 *        | Some n -> Int32.to_string n)
 *     (function
 *        | "" -> None
 *        | s -> Some (Int32.of_string s))
 *     None 8 true (fun _ -> ())
 * in
 * let importance_input =
 *   Js.Html.input
 *     (function
 *        | None -> ""
 *        | Some n -> Int32.to_string n)
 *     (function
 *        | "" -> None
 *        | s -> Some (Int32.of_string s))
 *     None 8 true (fun _ -> ())
 * in *)
  let form =
    Js.Html.div
      ~style:"position: fixed; left: 25px; bottom: 25px; \
      -moz-border-radius: 5px; padding: 10px; \
                                 background-color: white; text-align: right;"
      [ (* Title *) title_input.Js.Html.node ;
                               Js.Html.br () ;

(*       (* progress *)     Js.Html.string " Progress : " ;
 *                            progress_input.Js.Html.node ;
 *       (* importance *) Js.Html.string " Importance : " ;
 *                          importance_input.Js.Html.node ;
 *                                          Js.Html.br () ; *)
      ]
  in
  let close () =
    Js.Node.remove body form ;
    Js.Node.remove body mask
  in
  let save () =
    if !savable
    then
      begin
        try
          let args = (*TODO : use xml*)
            [ ("id", Int32.to_string id) ;
              ("title", title_input.Js.Html.get ()) ;
            ]
          in
            Js.alert ( snd (
              Js.http_post
                "http://localhost:8080/bsh/"
                "application/x-www-form-urlencoded"
                (Encode.urlencode args))) ;
            close ()
        with exc -> Js.alert ("unable to save task :\n"
                              ^ (Printexc.to_string exc)) ;
                    close ()
      end
    else Js.alert "Fill the description field before saving"
  in
  let save_close_node = 
    (Js.Html.div
       [
         Js.Html.a ~onclick:save
           [Js.Node.text "SAVE"] ;
         Js.Html.string " - " ;
         Js.Html.a ~onclick:close
           [Js.Node.text "CLOSE"] ;
       ]
    )
  in

    Js.Node.append form save_close_node ;
    Js.Node.append body mask ;
    Js.Node.append body form
;;


module Fields =
struct

  let send_editable_field args =
    let (code, msg) =
      Js.http_post "http://localhost:8080/bsh/"
        "application/x-www-form-urlencoded"
        (Encode.urlencode args)
    in
      if code / 100 = 2
      then ()
      else Js.alert msg

  let auto_update_version_deadline (parent, value, id) =
    let parent_node = Js.get_element_by_id parent in
    let input =
      Js.Html.input
        (fun v -> v) (fun v -> v) (* "conversions" *)
        value
        6                         (*size*)
        true                      (* editable *)
        (fun s ->
           (try
                let args =
                  [ ("__eliom_na__name","ocsforge_set_deadline_v");
                    ("id", Int32.to_string id) ;
                    ("deadline_v", s.Js.Html.get ()) ]
                in
                  send_editable_field args
              
            with exc -> (Js.alert (Printexc.to_string exc))) ;
           Js.Node.replace_all parent_node (Js.Node.text (s.Js.Html.get ()))
        )
    in
      Js.Node.replace_all parent_node input.Js.Html.node

  let auto_update_percent (parent, name, value, id) =
    let string_of_t = function
      | None -> ""
      | Some t -> string_of_int t
    in
    let t_of_string s =
      let t =
        match s with
          | "" -> None
          | s -> Some (int_of_string s)
      in
      if (match t with
            | None -> false
            | Some t -> t < 0 || 100 < t )
      then failwith "percent_of_string"
      else t
    in
    let input =
      Js.Html.input
        string_of_t t_of_string
        value
        4 true
        (fun s ->
           try
               send_editable_field [ ("id", Int32.to_string id) ;
                                     (name, string_of_t (s.Js.Html.get ())) ]
             
           with exc -> (Js.alert (Printexc.to_string exc)))
    in
      Js.Node.append (Js.get_element_by_id parent) input.Js.Html.node



end


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
  reg 189 pop_up_new_task ;
  reg 289 Row_color.color_fields ;
  reg 389 Fields.auto_update_version_deadline

