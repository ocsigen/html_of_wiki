open Js_of_ocaml

val get_fragment : Dom.node Js.t -> string option
val find_previous_heading : Dom.node Js.t -> Dom.node Js.t Js.Opt.t
val find_container : Dom.node Js.t -> Dom.node Js.t Js.Opt.t

type outline = section list
and section = Section of Dom.node Js.t list * string option * outline

val find_fragment : string -> section list -> outline

val outline :
  ?ignore:(Dom.node Js.t -> bool) -> Dom.node Js.t list -> section list

val build_ol : ?depth:int -> outline -> Dom_html.oListElement Js.t
