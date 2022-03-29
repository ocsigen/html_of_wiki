type t =
  | Dev
  | V of string * int list * string option

val to_string : t -> string
val parse : string -> t
val compare : t -> t -> int
