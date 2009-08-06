(* Ocsimore
 * Copyright (C) 2009
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

type patch_infos = {xml_author:string;
		    xml_email:string;
		    xml_date:string;
		    xml_local_date: string;
		    xml_inverted: string;
		    xml_hash: string}

type change = 
  | Add_dir of string
  | Rm_dir of string
  | Add_file of string
  | Rm_file of string
  | Modify_file of string
  | Move_file of (string*string)
    
type xml_patch = {xml_infos:patch_infos;
		  xml_name:string;
		  xml_comment:string;
		  xml_tree_changes:(change list) option}


type changeLog = xml_patch list
