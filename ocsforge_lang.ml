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

open CalendarLib
 (*TODO switch from Str to Netstring_pcre for the whole module*)

(*Working with options*)
let apply_on_opted f = function
  | None   -> None
  | Some v -> Some (f v)

let apply_on_opted_lwt f = function
  | None -> Lwt.return None
  | Some v -> Lwt.return (Some (f v))

let unopt ?(exc = Failure "Can't unopt None") ?default v =
  match (v,default) with
    | (Some x, _) -> x
    | (_, Some x) -> x
    | _           -> raise exc

let compare_opt ?(comp = compare) =
  (fun v1 v2 -> match (v1, v2) with
     | None,    Some _  -> (-1)
     | Some _,  None    -> 1
     | None,    None    -> 0
     | Some v1, Some v2 -> comp v1 v2)

let string_of_t_opt string_of_t =
  function
    | None -> ""
    | Some s -> string_of_t s

let t_opt_of_string t_of_string =
  (function
     | "" -> None
     | s  -> Some (t_of_string s))


(*List functions*)
let assoc_all k l =
  let rec aux accu = function
    | [] -> accu
    | (a,b)::t ->
        if a = k
        then aux (b::accu) t
        else aux accu t
  in aux [] l

let filter_map f l =
  let rec aux accu = function
    | [] -> accu
    | h::t -> (match f h with
                 | None -> aux accu t
                 | Some v -> aux (v::accu) t)
  in aux [] l

let insert_after l e reference =
  let rec aux accu = function
    | [] -> raise Not_found
    | h::t ->
        if h = reference
        then ( List.rev accu ) @ ( h :: e :: t )
        else aux (h :: accu) t
  in aux [] l

let insert_after_segment l e s =
  let rec aux = function
    | ([],[]) -> l @ [ e ]
    | ([],_ ) -> failwith "Not a segment of"
    | (tl,[]) -> s @ ( e :: tl )
    | (h1::t1,
       h2::t2) ->
         if h1 <> h2
         then failwith "Not a segment of"
         else aux (t1,t2)
  in aux (l,s)

let apply_on_uniq_or_fail s f = function
  | [ v ] -> f v
  | [] | _::_::_ -> failwith (s ^ " not one result")

let apply_on_uniq_or_fail_lwt s f = function
  | [ v ] -> Lwt.return (f v)
  | [] | _::_::_ -> failwith (s ^ " not one result")


(* auto list generation *)
let interval_list ?(comp = compare) ~bump ~min ~max () =
  let rec aux accu curr =
    if (comp curr max) > 0
    then accu
    else aux (curr::accu) (bump curr)
  in List.rev (aux [] min) (*TODO : optimize (easy) *)

let int_interval_list ?(bump = 1) ~min ~max () =
  interval_list ~bump:((+) bump) ~min ~max ()

let int32_interval_list ?(bump = Int32.one) ~min ~max () =
  interval_list ~bump:(Int32.add bump) ~min ~max ()

let date_interval_list ?(bump = Calendar.Date.Period.lmake ~day:1 ())
    ~min ~max () =
  interval_list
    ~comp:(Calendar.Date.compare)
    ~bump:(fun d -> Calendar.Date.add d bump)
    ~min ~max ()

let period_interval_list ?(bump = Calendar.Period.lmake ~hour:1 ())
    ~min ~max () =
  interval_list
    ~comp:(Calendar.Period.compare)
    ~bump:(Calendar.Period.add bump)
    ~min ~max ()


(*rougthly print period values*)
let string_of_period p =
  let h = int_of_float
            (Time.Period.to_hours
               (Calendar.Period.to_time p))
  in
  if h >= 24
  then
    let d = Date.Period.nb_days
              (Calendar.Period.to_date p)
    in
    if h < 72
    then
      let h = h mod 24 in
        if h = 0
        then
          Printf.sprintf "%i days" d
        else
          Printf.sprintf "%i days, %i hours" d (h mod 24)
    else
      Printf.sprintf "%i days" d
  else
    Printf.sprintf "%i hours" h

let period_of_string s =
  let days_re = Str.regexp "\\([0-9]+\\) days" in
  let hours_re = Str.regexp "\\([0-9]+\\) hours" in
  let hours =
    try
      let _ = Str.search_forward hours_re s 0 in
      int_of_string (Str.matched_group 1 s)
    with Not_found -> 0
  in
  let days =
    try
      let _ = Str.search_forward days_re s 0 in
      int_of_string (Str.matched_group 1 s)
    with Not_found -> 0
  in Calendar.Period.lmake ~day:days ~hour:hours ()

(* give the number of hours to go *)
let hours_in_period p = 24 * (Date.Period.nb_days (Calendar.Period.to_date p))

(*rougthly print date values*)
let string_of_date = Printer.Date.to_string

let date_of_string = Printer.Date.from_string

(* get the number of day until the given date is reached *)
let days_until d = Date.Period.nb_days (Date.sub d (Date.today ()))

(* class deadlines into 10 groups *)
let urgency d =
  let diff = Date.Period.nb_days
               (Calendar.Date.sub d
                 (Calendar.Date.today ())) in
    if diff < 0
    then 9
    else (match diff with
            | 0 -> 8
            | 1 -> 7
            | 2 -> 6
            | 3 -> 5
            | 4 | 5 -> 4
            | 6 | 7 -> 3
            | n when n > 7 && n <= 14 -> 2
            | n when n > 14 && n <= 30 -> 1
            | _ -> 0)


