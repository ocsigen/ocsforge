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

  let send url args =
    let (code, msg) =
      Js.http_post
        url
        "application/x-www-form-urlencoded"
        (Encode.urlencode args)
    in
      if code / 100 = 2
      then ()
      else Js.alert msg


module Fields =
struct

  let auto_update_input
        ~string_of_t ~t_of_string        (* conversion functions         *)
        ~value                           (* initial value                *)
        ?(size = 12) ?(editable = true)  (* shape and editability        *)
        ?(cb_first = (fun _ -> ()))      (* callback before the emission *)
        ?(cb_second = (fun _ -> ()))     (* callback after the emission  *)
        ~url                             (* url to send new values to    *)
        ~service                         (* name of the eliom service    *)
        ~args                            (* defalut arguments            *)
        ~param_name                      (* name of the argument         *)
        ()
      =
    Js.Html.input string_of_t t_of_string value size editable
      (fun s ->
         cb_first s ;
         begin
           try
             send url
               (   ("__eliom_na__name", service)
                :: (param_name, string_of_t (s.Js.Html.get ()))
                :: args )
           with exc -> Js.alert (Printexc.to_string exc)
         end ;
         cb_second s)


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
