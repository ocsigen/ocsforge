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
  | eof     { Eof(0) }  
  | default_token as d  { Default_lexer_token(d) }
