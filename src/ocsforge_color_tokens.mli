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

type lexemes = 
  | Comment of string*string*string (* open comment + comment text + close comment *)
  | Keyword of string
  | ITE of string (* if | then | else *)
  | Newline of int
  | Space of string
  | Tab of int
  | Int of string
  | Bin of string
  | Oct of string
  | Hex of string
  | Operator of string
  | Delimiter of string
  | Id of string
  | Char of string
  | String of string
  | UpperCaseID of string
  | Eof of int
  | Default_lexer_token of string
  | Unknown of char
