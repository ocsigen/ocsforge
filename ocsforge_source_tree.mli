open Ocsforge_source_types

val remove_node : 
    rep_tree ->
    rep_tree list ->
    rep_tree list

val find_node :
    string ->
    rep_tree list ->
    rep_tree

val insert :
    rep_tree ->
    string list ->
    rep_tree ->
    rep_tree

val delete : 
    rep_tree ->
    string list ->
    rep_tree ->
    rep_tree

val get_node : 
    string ->
    string list ->
    rep_tree ->
    rep_tree

val move : 
    string list ->
    string ->
    string list ->
    string ->
    rep_tree ->
    rep_tree

val update_infos : 
    string list ->
    string ->
    string ->
    string ->
    rep_tree ->
    rep_tree
