type lexemes = 
  | Comment of string*string*string (* open comment + comment text + close comment *)
  | Keyword of string
  | ITE of string (* if | then | else *)
  | Newline of char
  | Space of string
  | Tab of int
  | Int of string
  | Bin of string
  | Oct of string
  | Hex of string
  | Operator of string
  | Delimiter of string
  | Id of string
  | String of string
  | UpperCaseID of string
  | Eof of int
  | Default of char
