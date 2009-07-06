{
 open Lexing
 open Ocsforge_color_tokens
}

rule token = parse
  | ' '     { Space(1) } 
  | _ as c  { Default(c) }
  | eof     { Eof(0) }  
