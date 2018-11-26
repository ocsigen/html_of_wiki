open Utils.Operators

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



let bool ?err ?check ?default () =
  Bool_t {default = default <$> (fun d -> Bool d);
          check = check <$> (fun c -> Bool_checker c);
          err}

let string ?err ?check ?default () =
  String_t {default = default <$> (fun d -> String d);
            check = check <$> (fun c -> String_checker c);
            err}

let sequence ?err ?check ?default elt =
  Sequence_t (elt, {default = default <$> (fun d -> Sequence d);
                    check = check <$> (fun c -> Sequence_checker c);
                    err})

let mapping ?err ?check ?default pairs =
  Mapping_t (pairs, {default = default <$> (fun d -> Mapping d);
                     check = check <$> (fun c -> Mapping_checker c);
                     err})

let either ?err ?check ?default alt alt' =
  Either_t (alt, alt', {default;
                        check = check <$> (fun c -> Generic_checker c);
                        err})

let pair ?(doc = "") label tag elt =
  {label; tag; elt; doc}


(*
 * PARSERS
 *)
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

module Yaml_parser = struct
  type t = Yaml.value

  let parse ic =
    let content = Utils.read_in_channel ic in
    match Yaml.of_string content with
    | Result.Ok v -> v
    | Result.Error _ -> raise Parse_error

  let true_values = ["Y"; "y"; "true"; "True"; "TRUE"; "on"; "ON"]
  let false_values = ["N"; "n"; "false"; "False"; "FALSE"; "off"; "OFF"]
  let is_boolean_value x = List.mem x true_values || List.mem x false_values

  let elt_default = function
    | Bool_t {default}
    | String_t {default}
    | Sequence_t (_, {default})
    | Mapping_t (_, {default})
    | Either_t (_, _, {default}) -> default

  let rec structure_error_t_of_elt = function
    | Bool_t _ -> `Bool
    | String_t _ -> `String
    | Sequence_t _ -> `Sequence
    | Mapping_t _ -> `Mapping
    | Either_t (e, e', _) -> `Either (structure_error_t_of_elt e, structure_error_t_of_elt e')
  let rec structure_error_t_of_yaml = function
    | `Bool _ -> `Bool
    | `String _ -> `String
    | `O _ -> `Mapping
    | `A _ -> `Sequence
    | `Float _ -> `Float
    | `Null -> `Null
  let structure_error_of_conf_and_yaml conf yaml =
    let expected = structure_error_t_of_elt conf in
    let got = structure_error_t_of_yaml yaml in
    Structure_error {got; expected; inside = ""}

  let required_pairs ps = List.filter (fun {elt} -> Utils.is_none @@ elt_default elt) ps
  let pairs_labels ps = List.map (fun {label} -> label) ps
  let yaml_pairs_labels ps = List.map (fun (l, _) -> l) ps

  let rec structural_check conf yaml = match conf, yaml with
    | Bool_t _, `String s when is_boolean_value s -> ()
    | String_t _, `String _ -> ()
    | Sequence_t (elt, _), `A yamls -> List.iter (structural_check elt) yamls
    | Mapping_t (pairs, _), `O yaml_pairs ->
      let yaml_labels = yaml_pairs_labels yaml_pairs in
      let labels = pairs_labels pairs in
      let required_labels = pairs_labels @@ required_pairs pairs in
      (* let pl p l = Printf.printf "%s: [%s]\n" p (String.concat ", " l) in
       * pl "yaml_labels" yaml_labels; pl "labels" labels; pl "required_labels" required_labels; *)
      if yaml_labels @< labels
      then if required_labels @< yaml_labels
        then
          let pairs_alist = Utils.alist_of_values (fun {label} -> label) pairs in
          Utils.group_alists pairs_alist yaml_pairs
          |> List.iter (fun (_, ({label; elt}, yaml)) -> match structural_check elt yaml with
              | exception Structure_error {got; expected} -> raise (Structure_error {got; expected; inside = label})
              | x -> x)
        else raise (Missing_required_pairs (required_labels @- yaml_labels))
      else raise (Unexpected_labels (yaml_labels @- labels))
    | Either_t (elt, elt', _), yaml ->
      (try structural_check elt yaml
       with _ -> try structural_check elt' yaml
         with _ -> raise (structure_error_of_conf_and_yaml conf yaml))
    | _ -> raise (structure_error_of_conf_and_yaml conf yaml)

  let boolean_of_string = function
    | s when List.mem s true_values -> true
    | s when List.mem s false_values -> false
    | _ -> raise Not_matching
  let rec to_result conf yaml = match conf, yaml with
    | Bool_t _, `String s -> Bool (boolean_of_string s)
    | String_t _, `String s -> String s
    | Either_t (elt, elt', _), yaml ->
      (try to_result elt yaml
       with _ -> to_result elt' yaml)
    | Sequence_t (elt, _), `A yamls -> Sequence (List.map (to_result elt) yamls)
    | Mapping_t (pairs, _), `O yaml_pairs ->
      pairs
      |> List.map (fun {label; tag; elt} ->
          let default = elt_default elt in
          let value = List.assoc_opt label yaml_pairs <$> to_result elt in
          let value' = match value, default with
            | Some v, _ -> v
            | None, Some d -> d
            | None, None -> raise Not_matching
          in
          (tag, value'))
      |> (fun alist -> Mapping alist)
    | _ -> raise Not_matching


  let check_or_die check err value = if check value then () else raise (Check_error (err |? ""))
  let recursively_check k data = data |> List.iter (fun (elt, res) -> k elt res)

  let bool_check _ = function
    | Bool_t {check = Some (Bool_checker check); err}, Bool b -> check_or_die check err b
    | Bool_t {check = None}, Bool b -> ()
    | _ -> raise Not_matching

  let string_check _ = function
    | String_t {check = Some (String_checker check); err}, String s -> check_or_die check err s
    | String_t {check = None}, String s -> ()
    | _ -> raise Not_matching

  let sequence_check k =
    let rec_check elt results =
      recursively_check k (List.map (fun r -> (elt, r)) results)
    in
    function
    | Sequence_t (elt, {check = Some (Sequence_checker check); err}), Sequence results ->
      rec_check elt results;
      check_or_die check err results
    | Sequence_t (elt, {check = None}), Sequence results -> rec_check elt results
    | _ -> raise Not_matching

  let mapping_check k =
    let rec_check pairs map =
      recursively_check k (pairs |> List.map (fun {tag; elt} -> (elt, List.assoc tag map)))
    in
    function
    | Mapping_t (pairs, {check = Some (Mapping_checker check); err}), Mapping map ->
      rec_check pairs map;
      check_or_die check err map
    | Mapping_t (pairs, {check = None}), Mapping map -> rec_check pairs map
    | _ -> raise Not_matching

  let either_check k =
    let try_check elt elt' value =
      try recursively_check k [elt, value]
      with Not_matching -> recursively_check k [elt', value]
    in
    function
    | Either_t (elt, elt', {check = Some (Generic_checker check); err}), value ->
      try_check elt elt' value;
      check_or_die check err value
    | Either_t (elt, elt', {check = None}), value -> try_check elt elt' value
    | _ -> raise (Failure "either")

  let rec check_result conf result = match conf with
    | Bool_t _     -> bool_check     check_result (conf, result)
    | String_t _   -> string_check   check_result (conf, result)
    | Sequence_t _ -> sequence_check check_result (conf, result)
    | Mapping_t _  -> mapping_check  check_result (conf, result)
    | Either_t _   -> either_check   check_result (conf, result)
end

let yaml_parser = (module Yaml_parser : Parser with type t = Yaml.value)

let parse (type a) (module P : Parser with type t = a) elt ic =
  let psr_data = P.parse ic in
  P.structural_check elt psr_data;
  let result = P.to_result elt psr_data in
  P.check_result elt result;
  result


type 'a tag_printer = 'a -> string
type 'a backend_printer = 'a tag_printer -> 'a t -> string


let rec yaml_value_of_conf_t label_of_tag = function
  | Bool b -> `Bool b
  | String s -> `String s
  | Sequence seq -> `A (List.map (yaml_value_of_conf_t label_of_tag) seq)
  | Mapping map -> `O (map |> List.map (fun (tag, c) ->
      (label_of_tag tag, yaml_value_of_conf_t label_of_tag c)))

let yaml_printer tag_printer result =
  let yaml = yaml_value_of_conf_t tag_printer result in
  match Yaml.to_string yaml with
  | Result.Ok s -> s
  | Result.Error _ -> failwith "roh"


let tag_printer_of_conf conf =
  let rec lookup tag = function
    | Mapping_t (pairs, _) ->
      (try
         pairs
         |> List.find (fun {tag = tag'; elt} -> tag = tag')
         |> fun {label} -> label
       with Not_found ->
         pairs
         |> List.map (fun {elt} -> try Some (lookup tag elt) with Not_found -> None)
         |> List.filter Utils.is_some
         |> (function Some x :: _ -> x | _ -> raise Not_found))
    | Sequence_t (c, _) -> lookup tag c
    | Either_t (c, c', _) -> (try lookup tag c
                              with Not_found -> lookup tag c')
    | _ -> raise Not_found
  in
  fun tag -> lookup tag conf


let rec type_of_conf = function
  | Bool_t _ -> "boolean"
  | String_t _ -> "string"
  | Sequence_t (conf, _) -> "sequence of " ^ type_of_conf conf
  | Either_t (conf, conf', _) -> type_of_conf conf ^ " or " ^ type_of_conf conf'
  | Mapping_t _ -> "mapping"
and columns_of_mapping prefix = function
  | Mapping_t (pairs, _) ->
    pairs
    |> List.map (fun {label; elt; doc} ->
        (prefix ^ label, type_of_conf elt, doc) :: columns_of_subconf prefix elt)
    |> List.flatten
  | _ -> assert false
and columns_of_subconf prefix = function
  | Mapping_t _ as conf -> columns_of_mapping ("  " ^ prefix) conf
  | Either_t (c, c', _) -> columns_of_subconf prefix c @ columns_of_subconf prefix c'
  | _ -> []

let doc_of_conf conf = match conf with
  | Mapping_t _ -> columns_of_mapping "" conf |> Utils.sprint_three_cols
  | _ -> type_of_conf conf
