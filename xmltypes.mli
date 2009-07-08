type patch_infos = {xml_author:string;
		    xml_email:string;
		    xml_date:string;
		    xml_local_date: string;
		    xml_inverted: string;
		    xml_hash: string}

type change = 
  | Add_dir of string
  | Rm_dir of string
  | Add_file of string
  | Rm_file of string
  | Modify_file of string
  | Move_file of (string*string)
    
type xml_patch = {xml_infos:patch_infos;
		  xml_name:string;
		  xml_comment:string;
		  xml_tree_changes:(change list) option}


type changeLog = xml_patch list
