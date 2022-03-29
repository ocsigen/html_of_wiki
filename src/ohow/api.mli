type t

val parse_contents : string option -> t
val string_of_id : ?spacer:string -> t -> string
val index : t

module Ocamldoc : sig
  val fragment_of_id : t -> string option
  val path_of_id : ?prefix:string -> t -> string
end

module Odoc : sig
  val fragment_of_id : t -> string option
  val path_of_id : t -> string
end
