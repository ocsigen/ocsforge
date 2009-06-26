(* Ocsigen
 * http://www.ocsigen.org
 * ocamlfind/ocamlducefind wrapper
 * Copyright (C) 2009 St�phane Glondu
 * Laboratoire PPS - CNRS Universit� Paris Diderot
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation, with linking
 * exception; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

let rec argv_pick_duce res accu i =
  if i <= 0 then
    (res, accu)
  else if Sys.argv.(i) = "-notduce" then
    argv_pick_duce true accu (i-1)
  else
    argv_pick_duce res (Sys.argv.(i)::accu) (i-1)

let _ =
  let (notduce, argv) = argv_pick_duce false [] (Array.length Sys.argv-1) in
  let command = if notduce then "ocamlfind" else "ocamlducefind" in
  let argv = Array.of_list (command::argv) in
  Unix.execvp command argv
