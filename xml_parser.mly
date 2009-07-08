%{
open Xmltypes
%}

%token EOF
%token CHANGELOG_OPEN
%token CHANGELOG_CLOSE
%token CREATED_AS_OPEN
%token CREATED_AS_CLOSE
%token PATCH_OPEN
%token PATCH_CLOSE
%token SUMMARY_OPEN
%token SUMMARY_CLOSE
%token <string> ORIGINAL_NAME
%token <string*string> MOVE
%token <string*string> AUTHOR
%token <string> DATE
%token <string> LOCAL_DATE
%token <string> INVERTED
%token <string> HASH
%token <string> NAME
%token <string> COMMENT
%token <string> FILE_ADD
%token <string> DIR_ADD
%token <string> FILE_REMOVE
%token <string> DIR_REMOVE
%token <string> MODIFY_FILE

%start log
%type <Xmltypes.changeLog> log
%%

log:
| CHANGELOG_OPEN created_as* x=patch* CHANGELOG_CLOSE EOF
    { x }

created_as:
| CREATED_AS_OPEN n=ORIGINAL_NAME x=patch* CREATED_AS_CLOSE { n }

patch:
| PATCH_OPEN i=patch_infos n=NAME c=COMMENT s=summary? PATCH_CLOSE
    { {xml_infos=i; xml_name=n; xml_comment=c; xml_tree_changes=s} }
| PATCH_OPEN i=patch_infos n=NAME s=summary? PATCH_CLOSE
    { {xml_infos=i; xml_name=n; xml_comment=""; xml_tree_changes=s} }

patch_infos:
| a=AUTHOR d=DATE ld=LOCAL_DATE i=INVERTED h=HASH
    {{xml_author=(fst a); 
      xml_email=(snd a); 
      xml_date=d; 
      xml_local_date=ld; 
      xml_inverted=i; 
      xml_hash=h}}

summary: 
| SUMMARY_OPEN t=tree_changes* SUMMARY_CLOSE { t }

tree_changes:
| d=DIR_ADD      { Add_dir(d) }
| d=DIR_REMOVE   { Rm_dir(d) }
| f=FILE_ADD     { Add_file(f) } 
| f=FILE_REMOVE  { Rm_file(f) }
| f=MODIFY_FILE  { Modify_file(f) }
| mv=MOVE        { Move_file(fst mv,snd mv) }


