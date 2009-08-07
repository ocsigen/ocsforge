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

(** This module aggregates common functions. It has nothing specific to ocsforge and is probably already aviable in Batteries. *)

open CalendarLib

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
  | []      -> failwith (s ^ " no result")
  | _::_::_ -> failwith (s ^ " more than one result")

let apply_on_uniq_or_fail_lwt s f = function
  | [ v ] -> Lwt.return (f v)
  | []      -> failwith (s ^ " no result")
  | _::_::_ -> failwith (s ^ " more than one result")


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

let t_opt_list_of_t_list l = None :: (List.map (fun s -> Some s) l)


(*rougthly print period values*)

let string_of_period p = match Calendar.Period.ymds p with
  | (0, 0, 0, s) -> Printf.sprintf "%i hours" (s / (60 * 60))
  | (0, 0, n, _) -> Printf.sprintf "%i days" n
  | (0, m, _, _) -> Printf.sprintf "%i months" m
  | (y, _, _, _) -> Printf.sprintf "%i years" y

let period_of_string s =
  try Scanf.sscanf s "%i hours" (fun hour -> Calendar.Period.lmake ~hour ())
  with Scanf.Scan_failure _ -> (
  try Scanf.sscanf s "%i days" (fun day -> Calendar.Period.lmake ~day ())
  with Scanf.Scan_failure _ -> (
  try Scanf.sscanf s "%i months" (fun month -> Calendar.Period.lmake ~month ())
  with Scanf.Scan_failure _ -> (
  try Scanf.sscanf s "%i years" (fun year -> Calendar.Period.lmake ~year ())
  with Scanf.Scan_failure _ -> ( failwith "period_of_string" ) ) ) )


(* give the number of days/hours to go *)
let days_in_period p = Date.Period.nb_days (Calendar.Period.to_date p)
let hours_in_period p =
  (fun (_,_,d,s) -> (d * 24) + ((Calendar.Time.Second.to_int s) / (60 * 60)))
    (Calendar.Period.ymds p)

(*rougthly print date values*)
let string_of_date = Printer.Date.to_string

(* get the number of day until the given date is reached *)
let days_until d = Date.Period.nb_days (Date.sub d (Date.today ()))


module Tree =
struct

  type 'a tree = { content : 'a ; children : 'a tree list }
  exception Empty_tree

  let node n l = { content = n ; children = l }
  let get_content { content = n } = n
  let get_children { children = l } = l

  let iter f tree=
    let rec aux { content = t ; children = l } = f t l ; List.iter aux l in
      aux tree
  (** the function argument receives depth information *)
  let iteri f tree =
    let rec aux i { content = t ; children = l } =
      f t l i ;
        List.iter (aux (succ i)) l
    in aux 0 tree

  let find f tree =
    let rec aux { content = t ; children = l } =
      if f t l then node t l else auxaux l
    and auxaux = function
      | [] -> raise Not_found
      | hd::tl -> try aux hd with Not_found -> auxaux tl
    in aux tree

  let get_parent (tree : 'a tree) (n : 'a tree) : 'a tree =
    find (fun _ l -> List.mem n l) tree

  let get_depth (tree : 'a tree) (n : 'a tree) : int =
    let rec aux depth { content = t ; children = l } =
      if t = get_content n then depth else auxaux (succ depth) l
    and auxaux depth = function
      | [] -> raise Not_found
      | hd::tl -> try aux depth hd with Not_found -> auxaux (succ depth) tl
    in aux 0 tree

  let map f tree =
    let rec aux { content = t ; children = l } = 
      let (t,l) = f t l in
      let l = List.map aux l in
        node t l 
    in aux tree

  let filter f tree =
    let rec aux { content = t ; children = l } =
      if f t l then Some (node t  (filter_map aux l)) else None
    in
      unopt ~exc:Empty_tree (aux tree)

  let insert f tree n =
    map (fun t l -> (t, if f t l then n::l else l)) tree
  let insert_at tree n d =
    let d = get_content d in
    let rec aux { content = t ; children = l } =
      if t = d then node t (n :: l) else node t (auxaux [] l)
    and auxaux acc = function
      | [] -> List.rev acc
      | hd::tl -> auxaux ((aux hd) :: acc) tl
    in aux tree

  let move tree n d =
    insert_at (filter (fun nn _ -> nn <> n.content) tree) n d

  let to_list tree =
    let rec aux { content = t ; children = l } =
      t :: (List.flatten (List.map aux l))
    in aux tree

  let is_in_lineage parent child =
    let rec aux children =
      List.mem child children || auxaux children
    and auxaux = function
      | [] -> false
      | { children = c } :: tl -> aux c || auxaux tl
    in aux (get_children parent)

  let sort ?(comp = compare) t =
    let rec aux { content = n ; children = c } =
      node n (List.map aux (List.sort comp c))
    in aux t

end

