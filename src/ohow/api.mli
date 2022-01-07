type id

val parse_contents : string option -> id
val string_of_id : ?spacer:string -> id -> string
val fragment_of_id : id -> string option
val path_of_id : ?prefix:string -> id -> string
