type id

val parse_contents : string option -> id
val string_of_id : ?spacer:string -> id -> string

module Ocamldoc : sig
  val fragment_of_id : id -> string option
  val path_of_id : ?prefix:string -> id -> string
end

module Odoc : sig
  val fragment_of_id : id -> string option
  val path_of_id : id -> string
end
