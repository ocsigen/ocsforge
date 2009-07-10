{
 open Lexing
 open Ocsforge_color_tokens
}

rule token = parse
  | ' '+ as s     { Space(s) } 
  | eof     { Eof(0) }  
  | _* as d  { Default_lexer_token(d) }
