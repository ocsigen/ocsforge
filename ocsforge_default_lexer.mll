{
 open Lexing
 open Ocsforge_color_tokens
}

rule token = parse
  | ' '+ as s     { Space(s) } 
  | _ as c  { Default(c) }
  | eof     { Eof(0) }  
