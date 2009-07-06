(* This module factorizes some obrowser concept and patterns *)


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

  let percent_assoc = (*/!\ '%' must be first ; ' ' must be after '+' !*)
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

let send_post url args = (*TODO : return (code,msg) instead of interpretting them*)
  let (code, msg) =
    Js.http_post
      url
      "application/x-www-form-urlencoded"
      (Encode.urlencode args)
  in
    if code / 100 = 2
    then ()
    else Js.alert msg

let smart_create ~name ?(attrs = []) children =
  let m = Js.Html.create name ~attrs () in
    List.iter (Js.Node.append m) children ;
    m


module Fields =
struct

  let auto_update_input
        ~string_of_t ~t_of_string        (* conversion functions         *)
        ~value                           (* initial value                *)
        ?(size = 8) ?(editable = true)   (* shape and editability        *)
        ?cb_first                        (* callback before the emission *)
        ?cb_second                       (* callback after the emission  *)
        ~url                             (* url to send new values to    *)
        ~service                         (* name of the eliom service    *)
        ~args                            (* defalut arguments            *)
        ~param_name                      (* name of the argument         *)
        ()
      =
    Js.Html.input string_of_t t_of_string value size editable
      (let send s =
         try
           send_post url
             (   ("__eliom_na__name", service)
             :: (param_name, string_of_t (s.Js.Html.get ()))
             :: args )
         with exc -> Js.alert (Printexc.to_string exc)
       in
       let ( |> ) f g = (fun x -> f x ; g x) in
         match (cb_first, cb_second) with
           | (None    , None    ) ->        send
           | (Some fcb, None    ) -> fcb |> send
           | (None    , Some scb) ->        send |> scb
           | (Some fcb, Some scb) -> fcb |> send |> scb)

  let visual_percent
        ?(color = "rgb(100,100,255)")
        ?(bg_color = "rgb(230,230,230)")
        ~value
        ()
      =
    let value = min 100 (max 0 value) in
    Js.Html.table ~attrs:[("width", "100%")]
      [smart_create ~name:"colgroup"
         [smart_create ~name:"col"
                       ~attrs:[("width", (string_of_int value) ^ "%")]
                       [] ;
          smart_create ~name:"col"
                       ~attrs:[("width", (string_of_int (100 - value)) ^ "%")]
                       [] ;
         ] ;
        
       Js.Html.tr
         [Js.Html.td
            ~style:("background-color:" ^ color)
            ~attrs:[("align", "right")]
            [Js.Node.text
               (if value > 50
                then (string_of_int value) ^ "%"
                else "")] ;
          Js.Html.td
            ~style:("background-color:" ^ bg_color)
            ~attrs:[("align", "right")]
            [Js.Node.text
               (if value <= 50
                then (string_of_int (100 - value)) ^ "%"
                else "")] ;
         ] ;
      ]

end


module Opt =
struct

  let string_of_t_opt ?(none = "None") ?(quote = "") ?end_quote string_of_t =
    function
      | None -> none
      | Some v ->
          if quote = ""
          then string_of_t v
          else
            (  quote
             ^ (string_of_t v)
             ^ (match end_quote with
                  | None -> quote
                  | Some q -> q))

  let t_opt_of_string ?(none = "None") ?(quote = "") ?end_quote t_of_string =
    let end_quote = match end_quote with
      | None -> quote
      | Some q -> q
    in
    let ql = String.length quote
    and eql = String.length end_quote
    in
    fun s ->
      Js.alert ("t_opt_of_string : " ^ s ^ " with none, quote : " ^ none ^ " " ^ quote);
      let sl = String.length s in
      if s = none
      then None
      else (if String.sub s 0 ql = quote
            then (if String.sub s (pred (sl - eql)) eql = end_quote
                  then Some (t_of_string (String.sub s ql (sl -ql - eql)))
                  else (Js.alert "Obrowser_lang.t_opt_of_string invalid string";
                        failwith "Obrowser_lang.t_opt_of_string invalid string"))
            else (Js.alert "Obrowser_lang.t_opt_of_string invalid string";
                        failwith "Obrowser_lang.t_opt_of_string invalid string"))

  let apply_on_opted f = function
    | None   -> None
    | Some v -> Some (f v)

  let unopt ?default v =
    match (v,default) with
      | (Some x, _) -> x
      | (_, Some x) -> x
      | _           -> failwith "Can't unopt None"

  let compare_opt ?(comp = compare) =
    (fun v1 v2 -> match (v1, v2) with
       | None,    Some _  -> (-1)
       | Some _,  None    -> 1
       | None,    None    -> 0
       | Some v1, Some v2 -> comp v1 v2)

end
(*
module Events =
struct

  module type PARAMS = sig
    type v (* event valuation *)
    val name : string
    val destruct : obj -> v
    val default_value : v option
  (* an error message is produced if default value is None
   * and the destruction failed *)
  end

    module Make = functor (Params : PARAMS) ->
    struct
    open Params
    exception Cannot_destruct of exn
    let handlers_field = "caml_" ^ name ^ "_handlers"

    let bind f obj =
      let handlers =
        try
          Obj.obj (obj >>> get handlers_field >>> as_block)
with Failure "as_block" ->
  (* first event handler *)
  let handlers = ref [] in
    obj >>> set handlers_field (inject (Block (Obj.repr handlers))) ;
    obj >>> set name
      (wrap_event
                                                                       (fun evt ->
                                                                         let v =
                                                                           try destruct evt with e ->
                                                                             match default_value with
                                                                               | Some v -> v
                                                                               | None -> raise (Cannot_destruct e)
  in
    List.iter (fun f -> f v) !handlers)) ;
                                       handlers
  in handlers := f :: (List.filter ((!=) f) !handlers)

  let unbind f obj =
    let handlers =
      try
        Obj.obj (obj >>> get handlers_field >>> as_block)
      with Failure "as_block" ->
        ref []
    in
      handlers := List.filter ((!=) f) !handlers ;
      if !handlers = [] then (
        obj >>> set handlers_field (inject Nil) ;
        obj >>> set name (inject Nil)
      )

  let clear () obj =
    obj >>> set handlers_field (inject Nil) ;
    obj >>> set name (inject Nil)
end

end
 *)
