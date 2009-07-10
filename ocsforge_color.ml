open XHTML.M
open Eliom_predefmod.Xhtml
open Eliom_tools
open Eliom_services
open Lwt
open Netstring_str
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

(** permet d'ajouter des espaces en début de ligne en fonction du n° de ligne *)
(** non utilisé *)
let add_spaces numligne = 
  if (numligne>999) then " "
  else if (numligne>99) then "  "
  else if (numligne>9) then "   "
  else "    "

let rec color lexbuf fileName = match ((getLexer fileName) lexbuf) with
  | Comment(c_open,text,c_close) ->
     color lexbuf fileName >>= fun b ->
       Lwt.return 
	 ({{ [<span class="color_comment"> {: (c_open^text^c_close) :} !b] }} 
	    : {{ [ Xhtmltypes_duce.span* ] }})
 | Keyword(k) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_keyword"> {: k :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
      (*(XHTML.M.span 
	 ~a: [a_class ["colorKeyWord"]]
	 [XHTML.M.pcdata k])::(color lexbuf fileName)*)
 | ITE(t) ->
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_test"> {: t :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
 (*(XHTML.M.span 
 ~a: [a_class ["colorTest"]]
 [XHTML.M.pcdata t])::(color lexbuf fileName)*)
 | Newline(n) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: String.make 1 n :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata n)::(color lexbuf fileName)*)
 | Space(s) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: s :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata " ")::(color lexbuf fileName)*)
 | Tab(_) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: "      " :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata "      ")::(color lexbuf fileName)*)
 | Int(i) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_int"> {: i :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorInt"]]
    [XHTML.M.pcdata i])::(color lexbuf fileName)*)
 | Bin(bin) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_bin"> {: bin :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorBin"]]
    [XHTML.M.pcdata b])::(color lexbuf fileName)*)
 | Oct(o) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_oct"> {: o :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorOct"]]
    [XHTML.M.pcdata o])::(color lexbuf fileName)*)
 | Hex(h) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_hex"> {: h :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorHex"]]
    [XHTML.M.pcdata h])::(color lexbuf fileName)*)
 | Operator(o) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: o :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata o)::(color lexbuf fileName)*)
 | Delimiter(d) ->
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_delimiter"> {: d :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorDelimiter"]]
    [XHTML.M.pcdata d])::(color lexbuf fileName)*)
 | Id(i) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: i :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata i)::(color lexbuf fileName)*)
 | String(s) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_string"> {: ("\""^s^"\"") :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorString"]]
    [XHTML.M.pcdata ("\""^s^"\"")])::(color lexbuf fileName)*)
 | UpperCaseID(u) -> 
    color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span class="color_ucid"> {: u :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.span
    ~a: [a_class ["colorModule"]]
    [XHTML.M.pcdata m])::(color lexbuf fileName)*)
 | Default_lexer_token(s) -> 
     color lexbuf fileName >>= fun b ->
      Lwt.return 
        ({{ [<span> {: s :} !b] }} 
           : {{ [ Xhtmltypes_duce.span* ] }})
    (*(XHTML.M.pcdata (String.make 1 c))::(color lexbuf fileName)*)
 | Eof(_) -> Lwt.return {{ [] }}
    
let build_pre lexbuf fileName = 
  color lexbuf fileName >>= fun content ->
  Lwt.return {{ <pre class="color"> {: content :} }}
