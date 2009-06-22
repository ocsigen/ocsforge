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


(*Working with options*)
let apply_on_opted f = function
  | None   -> None
  | Some v -> Some (f v)

let unopt ?default v =
  match (v,default) with
    | (Some x, _) -> x
    | (_, Some x) -> x
    | _           -> failwith "Can't unopt None"

(*List functions*)
let assoc_all k l =
  let rec aux accu = function
    | [] -> accu
    | (a,b)::t ->
        if a = k
        then aux (b::accu) t
        else aux accu t
  in aux [] l



(* auto list generation *)
let interval_list ?(comp = compare) ~bump ~min ~max () =
  let rec aux accu curr =
    if (comp curr max) > 0
    then accu
    else aux (curr::accu) (bump curr)
  in aux [] min

let int_interval_list ?(bump = 1) ~min ~max () =
  interval_list ~bump:((+) bump) ~min ~max ()

let int32_interval_list ?(bump = Int32.one) ~min ~max () =
  interval_list ~bump:(Int32.add bump) ~min ~max ()

let date_interval_list ?(bump = Calendar.Period.lmake ~day:1 ())
    ~min ~max () =
  interval_list
    ~comp:(Calendar.compare)
    ~bump:(fun d -> Calendar.add d bump)
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
  if h > 24
  then
    let d = Date.Period.nb_days
              (Calendar.Period.to_date p)
    in Printf.sprintf "%i days and %i hours" d (h mod 24)
  else Printf.sprintf "%i hours" h
