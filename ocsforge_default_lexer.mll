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

{
 open Lexing
 open Ocsforge_color_tokens

 let newline lexbuf =
   lexbuf.lex_curr_p <- {
     lexbuf.lex_curr_p with
                          pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
                          pos_bol =  lexbuf.lex_curr_p.pos_cnum;
			}

}


let newline_char = ['\n' '\r']

let default_token = [^'\n' '\r']+

rule token = parse
  | ' '+ as s     { Space(s) }
  | newline_char
      { newline lexbuf;
	Newline lexbuf.lex_curr_p.pos_lnum }
  | eof     { Eof(lexbuf.lex_curr_p.pos_lnum) }
  | default_token as d  { Default_lexer_token(d) }
