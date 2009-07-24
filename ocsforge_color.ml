open XHTML.M
open Eliom_predefmod.Xhtml
open Eliom_tools
open Eliom_services
open Lwt
open Netstring_pcre
open Ocsforge_color_tokens

(** la liste d'association (extension reconnue,fonction de lexing associée)*)
let extList = ref []

let setLexer extName lexFun = 
  try 
    let _ = List.assoc extName (!extList) in
    ()
  with Not_found ->
    extList := ((extName,lexFun)::(!extList))
	
(** permet de récuperer la bonne fonction de lexing en fonction 
de l'extension du nom de fichier en argument *)
let getLexer fileName = 
  let splitName = split (regexp "\\.") fileName in
  let ext =
    if ((List.length splitName) > 1) then 
      List.nth splitName 1 
    else
      ""
  in
  try 
    List.assoc ext (!extList)
  with Not_found -> Ocsforge_default_lexer.token (* le Lexer par défaut *)

let rec generate_lines_num nblines total_nb = 
  if (nblines == 0) then 
    Lwt.return {{ [] }}
  else generate_lines_num (nblines-1) total_nb >>= fun b -> 
    Lwt.return ({{ [!b <span class="line_num"> 
      {: if (nblines != total_nb) then 
	((Int32.to_string (Int32.of_int nblines))^"\n")
      else
	((Int32.to_string (Int32.of_int nblines)))
	:} ] }} : {{ [Xhtmltypes_duce.span*] }})      

let rec color2 lexbuf lexer = match (lexer lexbuf) with
  | Comment(c_open,text,c_close) ->
     color2 lexbuf lexer >>= fun (a,b) ->
       let str = (c_open^text^c_close) in
       Lwt.try_bind
         (fun () -> Lwt.return (Ocamlduce.Utf8.make str))
         (fun utf8 -> 
           Lwt.return 
             ((a,{{ [<span class="color_comment"> 
               {: utf8 :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
         (function _ ->
           Lwt.return 
             ((a,{{ [<span class="color_comment"> 
               {: str :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
  | Keyword(k) -> 
      color2 lexbuf lexer >>= fun (a,b) ->
        Lwt.return 
          ((a,{{ [<span class="color_keyword"> {: k :} !b] }}) 
             : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
          (*(XHTML.M.span 
	    ~a: [a_class ["colorKeyWord"]]
	    [XHTML.M.pcdata k])::(color2 lexbuf lexer)*)
  | ITE(t) ->
      color2 lexbuf lexer  >>= fun (a,b) ->
        Lwt.return 
          ((a,{{ [<span class="color_test"> {: t :} !b] }}) 
             : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
          (*(XHTML.M.span 
            ~a: [a_class ["colorTest"]]
            [XHTML.M.pcdata t])::(color2 lexbuf lexer)*)
  | Newline(_) -> 
      color2 lexbuf lexer >>= fun (a,b) ->
        Lwt.return 
          ((a,{{ [<span> {: "\n" :} !b] }}) 
             : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
          (*(XHTML.M.pcdata n)::(color2 lexbuf lexer)*)
  | Space(s) -> 
      color2 lexbuf lexer  >>= fun (a,b) ->
        Lwt.return 
          ((a,{{ [<span> {: s :} !b] }})
             : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
          (*(XHTML.M.pcdata " ")::(color2 lexbuf lexer)*)
  | Tab(_) -> 
      color2 lexbuf lexer  >>= fun (a,b) ->
        Lwt.return 
          ((a,{{ [<span> {: "      " :} !b] }})
             : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
          (*(XHTML.M.pcdata "      ")::(color2 lexbuf lexer)*)
 | Int(i) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_int"> {: i :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorInt"]]
    [XHTML.M.pcdata i])::(color2 lexbuf lexer)*)
 | Bin(bin) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_bin"> {: bin :} !b] }}) 
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorBin"]]
    [XHTML.M.pcdata b])::(color2 lexbuf lexer)*)
 | Oct(o) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_oct"> {: o :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorOct"]]
    [XHTML.M.pcdata o])::(color2 lexbuf lexer)*)
 | Hex(h) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_hex"> {: h :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorHex"]]
    [XHTML.M.pcdata h])::(color2 lexbuf lexer)*)
 | Operator(o) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span> {: o :} !b] }}) 
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.pcdata o)::(color2 lexbuf lexer)*)
 | Delimiter(d) ->
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_delimiter"> {: d :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorDelimiter"]]
    [XHTML.M.pcdata d])::(color2 lexbuf lexer)*)
 | Id(i) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span> {: i :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.pcdata i)::(color2 lexbuf lexer)*)
 | String(s) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      let str = ("\""^s^"\"") in 
      Lwt.try_bind
        (fun () -> Lwt.return (Ocamlduce.Utf8.make str))
        (fun utf8 -> 
          Lwt.return 
            ((a,{{ [<span class="color_string"> 
              {: utf8 :} !b] }}) 
               : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
        (function _ ->
          Lwt.return 
            ((a,{{ [<span class="color_string"> 
              {: str :} !b] }}) 
               : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
    (*(XHTML.M.span
    ~a: [a_class ["colorString"]]
    [XHTML.M.pcdata ("\""^s^"\"")])::(color2 lexbuf lexer)*)
 | UpperCaseID(u) -> 
    color2 lexbuf lexer  >>= fun (a,b) ->
      Lwt.return 
        ((a,{{ [<span class="color_ucid"> {: Ocamlduce.Utf8.make u :} !b] }})
           : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }}))
    (*(XHTML.M.span
    ~a: [a_class ["colorModule"]]
    [XHTML.M.pcdata m])::(color2 lexbuf lexer)*)
 | Default_lexer_token(s) -> 
     color2 lexbuf lexer  >>= fun (a,b) ->
       Lwt.try_bind
         (fun () -> Lwt.return (Ocamlduce.Utf8.make s))
         (fun utf8 ->
           Lwt.return 
             ((a,{{ [<span> 
                      {: utf8 :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
         (function _ ->
           Lwt.return 
             ((a,{{ [<span> 
                      {: s :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
 | Unknown(c) ->
     color2 lexbuf lexer  >>= fun (a,b) ->
       let s = String.make 1 c in
       Lwt.try_bind 
         (fun () -> Lwt.return (Ocamlduce.Utf8.make s))
         (fun utf8 -> 
           Lwt.return 
             ((a,{{ [<span> 
                      {: utf8 :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
         (function _ ->
           Lwt.return 
             ((a,{{ [<span> 
                      {: s :} !b] }}) 
                : ({{[Xhtmltypes_duce.span*]}}*{{ [Xhtmltypes_duce.span*] }})))
         (*(XHTML.M.pcdata (String.make 1 c))::(color2 lexbuf lexer )*)
 | Eof(n) -> 
     generate_lines_num (n-1) (n-1) >>= fun b ->
       Lwt.return (b,{{ [] }})
    


let color lexbuf fileName = color2 lexbuf (getLexer fileName)
