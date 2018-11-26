type label = string
type doc = string

type 'a result =
  | Bool of bool_result
  | String of string_result
  | Sequence of 'a sequence_result
  | Mapping of 'a mapping_result
and bool_result = bool
and string_result = string
and 'a sequence_result = 'a result list
and 'a mapping_result = ('a * 'a result) list

type err = string
type 'a checker_t = 'a -> bool
type 'a checker =
  | Bool_checker of bool_result checker_t
  | String_checker of string_result checker_t
  | Sequence_checker of 'a sequence_result checker_t
  | Mapping_checker of 'a mapping_result checker_t
  | Generic_checker of 'a result checker_t

type 'a elt_meta = {
  default: 'a result option;
  check: 'a checker option;
  err: err option
}
type 'a elt =
  | Bool_t of 'a elt_meta
  | String_t of 'a elt_meta
  | Sequence_t of 'a elt * 'a elt_meta
  | Mapping_t of 'a mapping_pair list * 'a elt_meta
  | Either_t of 'a elt * 'a elt * 'a elt_meta
and 'a mapping_pair = {
  label: label;
  tag: 'a;
  elt: 'a elt;
  doc: doc;
}

type 'a t = 'a result


val bool :
  ?err:err ->
  ?check:(bool_result checker_t) ->
  ?default:bool_result ->
  unit -> 'a elt
val string :
  ?err:err ->
  ?check:(string_result checker_t) ->
  ?default:string_result ->
  unit -> 'a elt
val sequence :
  ?err:err ->
  ?check:('a sequence_result checker_t) ->
  ?default:'a sequence_result ->
  'a elt -> 'a elt
val mapping :
  ?err:err ->
  ?check:('a mapping_result checker_t) ->
  ?default:'a mapping_result ->
  'a mapping_pair list -> 'a elt
val either :
  ?err:err ->
  ?check:('a result checker_t) ->
  ?default:'a result ->
  'a elt -> 'a elt -> 'a elt

val pair : ?doc:doc -> label -> 'a -> 'a elt -> 'a mapping_pair

exception Parse_error
type structure_error_t = [ `Bool
                         | `Sequence
                         | `String
                         | `Mapping
                         | `Either of structure_error_t * structure_error_t
                         | `Float
                         | `Null ]
exception Structure_error of {got: structure_error_t;
                              expected: structure_error_t;
                              inside: label}
exception Check_error of err
exception Missing_required_pairs of label list
exception Unexpected_labels of label list
exception Not_matching

module type Parser = sig
  type t
  val parse : in_channel -> t
  val structural_check : 'a elt -> t -> unit
  val to_result : 'a elt -> t -> 'a result
  val check_result : 'a elt -> 'a result -> unit
end

module Yaml_parser : Parser
val yaml_parser : (module Parser with type t = Yaml.value)

val parse : (module Parser with type t = 'b) -> 'a elt -> in_channel -> 'a result



type 'a tag_printer = 'a -> string
type 'a backend_printer = 'a tag_printer -> 'a t -> string

val yaml_printer : 'a backend_printer

val tag_printer_of_conf : 'a elt -> 'a tag_printer


val doc_of_conf : 'a elt -> string
