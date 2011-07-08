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

(** @author Granarolo Jean-Henri *)

open Eliom_pervasives
open Eliom_tools
open Eliom_services
open Netstring_pcre
open Ocsforge_color_tokens
open HTML5.M

(** la liste d'association (extension reconnue,fonction de lexing associée)*)
let extList = ref []

(** la liste d'association (nom de langage,fonction de lexing associée)*)
let langList = ref []

let setLexer extName lexFun assoc_list =
  try
    let _ = List.assoc extName (!assoc_list) in
    ()
  with Not_found ->
    assoc_list := ((extName,lexFun)::(!assoc_list))

(** permet de récuperer la bonne fonction de lexing en fonction
de l'extension du nom de fichier en argument *)
let getLexer fileName assoc_list =
  let splitName = split (regexp "\\.") fileName in
  let key =
    if ((List.length splitName) > 1) then
      List.nth splitName 1
    else
      fileName
  in
  try
    List.assoc key (!assoc_list)
  with Not_found -> Ocsforge_default_lexer.token (* le Lexer par défaut *)

let register_ext extName lexFun = setLexer extName lexFun extList
let get_ext_lexer fileName = getLexer fileName extList

let register_lang extName lexFun = setLexer extName lexFun langList
let get_lang_lexer langName = getLexer langName langList

let rec generate_lines_num nblines total_nb =
  if (nblines == 0) then
    Lwt.return []
  else
    lwt b = generate_lines_num (nblines-1) total_nb in
    Lwt.return (b
		@[ span ~a:[a_class ["line_num"]]
		     [pcdata
			 (if (nblines != total_nb) then
			     ((Int32.to_string (Int32.of_int nblines))^"\n")
			  else
			     ((Int32.to_string (Int32.of_int nblines))))]])

let rec color2 lexbuf lexer = match (lexer lexbuf) with
  | Comment(c_open,text,c_close) ->
    lwt (a,b) = color2 lexbuf lexer in
    let str = (c_open^text^c_close) in
    Lwt.return ( a, ( span ~a:[a_class ["color_comment"]] [pcdata str]) :: b )
  | Keyword(k) ->
    lwt (a,b) = color2 lexbuf lexer in
    Lwt.return ( a, ( span ~a:[a_class ["color_keyword"]] [pcdata k]) :: b )
  | ITE(t) ->
    lwt (a,b) = color2 lexbuf lexer in
    Lwt.return ( a, ( span ~a:[a_class ["color_test"]] [pcdata t] ) :: b )
  | Newline(_) ->
    lwt (a,b) = color2 lexbuf lexer in
    Lwt.return ( a, ( span [pcdata "\n"] ) :: b )
  | Space(s) ->
    lwt (a,b) = color2 lexbuf lexer in
    Lwt.return ((a, ( span [pcdata s ]) :: b ))
  | Tab(_) ->
    lwt (a,b) = color2 lexbuf lexer in
    Lwt.return (a, ( span [pcdata "      " ]) :: b )
 | Int(i) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_int"]] [ pcdata i ] ) :: b )
 | Bin(bin) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_bin"]] [ pcdata bin ] ) :: b)
 | Oct(o) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_oct"]] [ pcdata o ] :: b ))
 | Hex(h) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_hex"]] [ pcdata h ] ) :: b )
 | Operator(o) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span [pcdata o ]) :: b )
 | Delimiter(d) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_delimiter"]] [ pcdata d ] ) :: b )
 | Id(i) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span [pcdata i ]) :: b )
 | Char(c) ->
   lwt (a,b) = color2 lexbuf lexer in
   let char = ("\'"^c^"\'") in
   Lwt.return (a, ( span ~a:[a_class ["color_char"]] [ pcdata char ] ) :: b )
 | String(s) ->
   lwt (a,b) = color2 lexbuf lexer in
   let str = ("\""^s^"\"") in
   Lwt.return (a, ( span ~a:[a_class ["color_string"]] [pcdata str] ) :: b )
 | UpperCaseID(u) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span ~a:[a_class ["color_ucid"]] [ pcdata u ] ) :: b )
 | Default_lexer_token(s) ->
   lwt (a,b) = color2 lexbuf lexer in
   Lwt.return (a, ( span [pcdata s ]) :: b )
 | Unknown(c) ->
   lwt (a,b) = color2 lexbuf lexer in
   let s = String.make 1 c in
   Lwt.return (a, ( span [pcdata s ]) :: b )
 | Eof(n) ->
   lwt b = generate_lines_num (n-1) (n-1) in
   Lwt.return (b, [] )



let color_by_ext lexbuf fileName = color2 lexbuf (get_ext_lexer fileName)
let color_by_lang lexbuf lang_name = color2 lexbuf (get_lang_lexer lang_name)
