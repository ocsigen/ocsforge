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


type 'a aux = 'a -> bool Lwt.t

class type area_rights =
object
  method forum_rights : Forum.wiki_rights

  method can_set_kinds : right_area aux
  method can_set_verion : right_area aux
  method can_create_subareas : right_area aux
  method can_move_tasks : right_area aux
  method can_move_tasks_to : right_area aux
  method can_move_tasks_from : right_area aux
  method can_administrate_tasks : right_area aux
  method can_edit_tasks_properties : right_area aux
  method can_edit_tasks_messages : right_area aux
  method can_edit_tasks_messages_when_authored : right_area aux
  method can_create_tasks : right_area aux
  method can_write_comment : right_area aux
  method can_write_comment_not_moderated : right_area aux
  method can_delete_comments : right_area aux
  method can_make_comments_sticky : right_area aux
  method can_moderate_comments : right_area aux
  method can_read_comments : right_area aux
  method can_read_tasks : right_area aux
end

