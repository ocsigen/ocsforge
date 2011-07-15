type src_page_kind =
    [ `Annot | `Browse | `Cat | `Diff | `Error | `Options | `PatchDiff ]
type log_page_kind = [ `Log of string option * string option ]
type page_kind =
    [ `Annot
    | `Browse
    | `Cat
    | `Diff
    | `Error
    | `Log of string option * string option
    | `Options
    | `PatchDiff ]
val kind_to_string :
  [< `Annot | `Browse | `Cat | `Diff | `Error | `Options | `PatchDiff ] ->
  string
val string_to_kind :
  string ->
  [> `Annot | `Browse | `Cat | `Diff | `Error | `Options | `PatchDiff ]
type project_services = {
  sources_service :
    (string list * (src_page_kind option * (string option * string option)),
     unit,
     [ `Attached of
         (Eliom_services.attached_service_kind, [ `Get ]) Eliom_services.a_s ],
     [ `WithSuffix ],
     [ `One of string list ] Eliom_parameters.param_name *
     ([ `One of src_page_kind ] Eliom_parameters.param_name *
      ([ `One of string ] Eliom_parameters.param_name *
       [ `One of string ] Eliom_parameters.param_name)),
     unit, [ `Registrable ], Eliom_output.appl_service)
    Eliom_services.service;
  log_service :
    ((string option * string option) option, unit,
     [ `Attached of
         (Eliom_services.attached_service_kind, [ `Get ]) Eliom_services.a_s ],
     [ `WithoutSuffix ],
     [ `One of string option * string option ] Eliom_parameters.param_name,
     unit, [ `Registrable ], Eliom_output.appl_service)
    Eliom_services.service;
}
val repos_services_table : (string, project_services) Hashtbl.t
val add_service : string -> project_services -> unit
val find_service : string -> project_services option
val find_sources_service : string ->
    (string list * (src_page_kind option * (string option * string option)),
     unit,
     [> `Attached of
         (Eliom_services.attached_service_kind, [ `Get ]) Eliom_services.a_s ],
     [> `WithSuffix ],
     [ `One of string list ] Eliom_parameters.param_name *
     ([ `One of src_page_kind ] Eliom_parameters.param_name *
      ([ `One of string ] Eliom_parameters.param_name *
       [ `One of string ] Eliom_parameters.param_name)),
     unit, [> `Registrable ], Eliom_output.appl_service)
    Eliom_services.service
val find_log_service : string ->
    ((string option * string option) option, unit,
     [> `Attached of
         (Eliom_services.attached_service_kind, [ `Get ]) Eliom_services.a_s ],
     [> `WithoutSuffix ],
     [ `One of string option * string option ] Eliom_parameters.param_name,
     unit, [> `Registrable ], Eliom_output.appl_service)
    Eliom_services.service
