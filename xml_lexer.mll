{
 open Lexing
 open Xml_parser
}

let blank = [' ' '\t' '\n' '\r']

let data = [^'<' '>']* 
let name = [^'<' '>' '\n' ' ' '\t' '\r']

let ignored = 
    ("<added_lines" data* "/>") 
  | ("<removed_lines" data* "/>")
  | ("Ignore-this: " name*)

rule token = parse
  | eof  
      { EOF }
  | blank* "<changelog>"                    
      { CHANGELOG_OPEN }
  | blank* "<created_as "                    
      { CREATED_AS_OPEN }
  | blank* "<patch "                            
      { PATCH_OPEN }
  | blank* "<name>" (data as n) "</name>"        
      { NAME(n)}
  | blank* "<comment>" ignored* (data as c) "</comment>"   
      { COMMENT(c) }
  | blank* "<summary>"          
      { SUMMARY_OPEN }
  | blank*"<modify_file>" blank* (name* as f) ignored* blank* "</modify_file>" 
      { MODIFY_FILE(f) }
  | blank* "<add_file>" blank* (name* as f) blank* "</add_file>"        
      { FILE_ADD(f) }
  | blank* "<add_directory>" blank* (name* as d) blank* "</add_directory>"
      { DIR_ADD(d) }
  | blank* "<remove_file>" blank* (name* as f) blank* "</remove_file>" 
      { FILE_REMOVE(f) }
  | blank* "<remove_directory>" blank* (name* as d) blank* "</remove_directory>" 
      { DIR_REMOVE(d) }
  | "original_name='"
         {
	  let n_start = lexeme_start_p lexbuf in
	  let n_buf = Buffer.create 10 in
	  let name = patch_field n_start n_buf lexbuf in
	  lexbuf.lex_start_p <- n_start;
	  ORIGINAL_NAME(name)
	}
  | "author='"    
         {
	  let aut_start = lexeme_start_p lexbuf in
	  let name_buf = Buffer.create 10 in
	  let mail_buf = Buffer.create 10 in
	  let (name,mail) = author aut_start 0 name_buf mail_buf lexbuf in
	  lexbuf.lex_start_p <- aut_start;
	  AUTHOR(name,mail)
	 }
  | "date='" 
         {
	  let date_start = lexeme_start_p lexbuf in
	  let buf = Buffer.create 10 in
	  let date = patch_field date_start buf lexbuf in
	  lexbuf.lex_start_p <- date_start;
	  DATE(date)
	 }     
  | "local_date='"
         {
	  let ldate_start = lexeme_start_p lexbuf in
	  let buf = Buffer.create 10 in
	  let ldate = patch_field ldate_start buf lexbuf in
	  lexbuf.lex_start_p <- ldate_start;
	  LOCAL_DATE(ldate)
	 }   
  | "inverted='"
         {
	  let inv_start = lexeme_start_p lexbuf in
	  let buf = Buffer.create 10 in
	  let inverted = patch_field inv_start buf lexbuf in
	  lexbuf.lex_start_p <- inv_start;
	  INVERTED(inverted)
	 }
  | "hash='"
         {
	  let hash_start = lexeme_start_p lexbuf in
	  let buf = Buffer.create 10 in
	  let hash = patch_field hash_start buf lexbuf in
	  lexbuf.lex_start_p <- hash_start;
	  HASH(hash)
	}       
  | blank* "<move " 
        {
	 let move_start = lexeme_start_p lexbuf in
	 let from_buf = Buffer.create 10 in
	 let to_buf = Buffer.create 10 in
	 let mv = move move_start 0 from_buf to_buf lexbuf in
	 MOVE(mv)
        }
  | blank* "</changelog>"   { CHANGELOG_CLOSE }
  | blank* "</created_as>"  { CREATED_AS_CLOSE }
  | blank* "</patch>"       { PATCH_CLOSE }
  | blank* "</summary>"     { SUMMARY_CLOSE }
  
  
and author start flag name_buf mail_buf = parse
  | "&gt" { author start flag name_buf mail_buf lexbuf } 
  | "'" blank*   { (Buffer.contents name_buf,Buffer.contents mail_buf) }
  | "&lt;"    { author start 1 name_buf mail_buf lexbuf }
  |  _ as c   { 
                if (flag == 0) then begin
		  Buffer.add_char name_buf c;
		  author start flag name_buf mail_buf lexbuf
		end
		else begin
		  Buffer.add_char mail_buf c;
		  author start flag name_buf mail_buf lexbuf
		end
              }

and patch_field start buf = parse 
  | "'>"  | "'" blank* { Buffer.contents buf }
  | _ as c  { Buffer.add_char buf c; 
	      patch_field start buf lexbuf }

and move start flag from_buf to_buf = parse
  | "\"/>"        { (Buffer.contents from_buf, Buffer.contents to_buf) }
  | "from=\""     { move start 0 from_buf to_buf lexbuf }
  | "to=\""       { move start 1 from_buf to_buf lexbuf }
  | "\"" 
  | " "           { move start flag from_buf to_buf lexbuf }
  | _ as c        { 
                    if (flag == 0) then begin
		      Buffer.add_char from_buf c;
		      move start flag from_buf to_buf lexbuf
		    end
		    else begin
		      Buffer.add_char to_buf c;
		      move start flag from_buf to_buf lexbuf
		    end
		  }
