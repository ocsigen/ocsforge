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

open Ocsforge_source_types

val remove_node :
    rep_tree ->
    rep_tree list ->
    rep_tree list

val find_node :
    string ->
    rep_tree list ->
    rep_tree

val insert :
    rep_tree ->
    string list ->
    rep_tree ->
    rep_tree

val delete :
    rep_tree ->
    string list ->
    rep_tree ->
    rep_tree

val get_node :
    string ->
    string list ->
    rep_tree ->
    rep_tree option

val move :
    string list ->
    string ->
    string list ->
    string ->
    rep_tree ->
    rep_tree

val update_infos :
    string list ->
    string ->
    string ->
    string ->
    string ->
    rep_tree ->
    rep_tree
