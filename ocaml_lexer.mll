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

let int = ['0'-'9']+
let hex = ("0x"| "0X") ['0'-'9' 'A'-'F' 'a'-'f']+
let oct = ("0o"| "0O") ['0'-'7']+   
let bin = ("0b"| "0B") ['0'-'1']+ 

let keyword = (
   "and" | "as" | "assert" | "asr" | "begin" | "class"
  | "constraint" | "do" | "done" | "downto" | "end"
  | "exception"  | "external" | "false" | "for" | "fun" | "function"
  | "functor" | "in" | "include" | "inherit" | "initializer"
  | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr"
  | "lxor" | "match" | "method" | "mod" | "module" | "mutable"
  | "new" | "object" | "of" | "open" | "or" | "private"
  | "rec" | "sig" | "struct" | "to" | "true"
  | "try" | "with" | "type" | "val" | "virtual" | "when" | "while" 
  | "!="  | "#"  | "&&" | "'" |  ","
  | "-."  | "->" | ".." | "::" | ":=" | ":>" | ";" | ";;"
  | "<-"  | ">]" | ">}" | "?" | "??" | "[<" | "[>"
  | "_" | "`" | "{<" | "|]" |  "~"
)
  
let ite = ( "if" | "then" | "else")
            
let operator = (
"<>" | "!" | "$" | "%" | "&" | "*" | "+" | "-" |  "." | "/"
| ":"| "<" | "=" | ">" | "?" | "@" | "^" | "|" | "~"
)

let delimiter = ("{" | "}" | "(" | ")" | "[" | "]" | "( " )

let comment = ("(*" | "(**")

let newline_char = ['\n' '\r']

let alpha = ['a'-'z''A'-'Z''0'-'9''_']*

let spaces = [' ']+

let default = [^' ' '\n' '\r']

rule token = parse
  | spaces as sp 
      { Space(sp) }
  | newline_char
      { newline lexbuf;
	Newline lexbuf.lex_curr_p.pos_lnum }
  | keyword as k             
      { Keyword k }
  | delimiter as d 
      { Delimiter d }
  | ite as t
      { ITE t }
  | int as i             
      { Int i  }
  | bin as b             
      { Bin b }
  | oct as o             
      { Oct o }
  | hex as h             
      { Hex h }
  | (['A'-'Z'](alpha)) as m  
      { UpperCaseID m } 
  | (['a'-'z'](alpha)) as c  
      { Id c }
  | operator as o
      { Operator o }
  | eof                      
      { Eof(0) }
  | '\"'
      { let string_start = lexeme_start_p lexbuf in
        let s = (string string_start (Buffer.create 10) lexbuf) in
        lexbuf.lex_start_p <- string_start;
        String s }
  | comment as c_open
      { let comment_start = lexeme_start_p lexbuf in
        let (text,c_close) = (comment comment_start (Buffer.create 10) lexbuf) in
        lexbuf.lex_start_p <- comment_start;
        Comment (c_open,text,c_close) }
  | _ as c 
      { Unknown(c) }
  


and comment start buf = parse
  | "\\n"
      { Buffer.add_char buf '\n';
        comment start buf lexbuf }
  | "\\\""
      { Buffer.add_char buf '\"';
        comment start buf lexbuf }
  | "*)"
      { (Buffer.contents buf,"*)") }
  | "**)"
      { (Buffer.contents buf,"**)")}
  | newline_char
      { newline lexbuf;
	Buffer.add_char buf '\n';
        comment start buf lexbuf }
  | _ as c
      { Buffer.add_char buf c;
        comment start buf lexbuf }
  | eof
      { (Buffer.contents buf, "") }

and string start buf = parse
   | "\\n"
      { Buffer.add_char buf '\\';
	Buffer.add_char buf 'n';
        string start buf lexbuf }
  | "\\\""
      { Buffer.add_char buf '\\';
	Buffer.add_char buf '\"';
        string start buf lexbuf }
  | '"'
      { Buffer.contents buf }
  | newline_char
      { newline lexbuf;
        Buffer.add_char buf '\n';
        string start buf lexbuf }
  | _ as c
      { Buffer.add_char buf c;
        string start buf lexbuf }
  | eof
      { "" }

{
Ocsforge_color.setLexer "ml" token;
Ocsforge_color.setLexer "mli" token;
Ocsforge_color.setLexer "mll" token;
Ocsforge_color.setLexer "mly" token
}
