(** Eliom module that creates a new forum *)

open Lwt

type forum_data =
    {
      title:string;
      descr:string;
      arborescent: bool;
    }

let default_forum_data =
  {
    title = "Forum";
    descr = "";
    arborescent = true;
  }

let forum_data =
  let rec find_forum_data data = function
    | [] -> Lwt.return data
    | (Simplexmlparser.Element ("name", [], s))::l -> 
        let name = Ocsigen_parseconfig.parse_string s in
        find_forum_data 
          {data with title = name}
          l
    | (Simplexmlparser.Element ("descr", [], s))::l -> 
        let name = Ocsigen_parseconfig.parse_string s in
        find_forum_data 
          {data with descr = name}
          l
    | (Simplexmlparser.Element ("notarborescent", [], []))::l -> 
        find_forum_data 
          {data with arborescent = false}
          l
    | (Simplexmlparser.Element (s, _, _))::_ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content <"^s^"> inside Ocsicreateforum config"))
    | _ ->
        Lwt.fail (Ocsigen_extensions.Error_in_config_file
                       ("Unexpected content inside Ocsicreateforum config"))
  in
  let c = Eliom_sessions.get_config () in
  Lwt_unix.run (find_forum_data default_forum_data c)

let forum_name_duce = Ocamlduce.Utf8.make forum_data.title


let _ =
  Lwt_unix.run (
     Forum.create_forum
       ~wiki_model:Ocsisite.wikicreole_model
       ~title:forum_data.title
       ~descr:forum_data.descr
       ~arborescent:forum_data.arborescent
       ()
    )


