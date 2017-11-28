open Ocsimore_lib
exception Error of string

let is_capitalized s =
  String.length s >= 1 &&
  let c = int_of_char s.[0] in
  int_of_char 'A' <= c && c <= int_of_char 'Z'

let check_capitalized_path path =
  List.iter
    (fun name -> if not (is_capitalized name) then
      raise (Error (Printf.sprintf "%S is not a valid module name" name)))
    path

let parse_lid id =
  match List.rev (Re.split dot (String.concat "" id)) with
  | id :: rpath when not (is_capitalized id) ->
    check_capitalized_path rpath;
    (List.rev rpath, id)
  | _ -> raise (Error (Printf.sprintf "invalid ocaml id %S" (String.concat "" id)))

let parse_uid id =
  match List.rev (Re.split dot (String.concat "" id)) with
  | id :: rpath when is_capitalized id ->
    check_capitalized_path rpath;
    (List.rev rpath, id)
  | _ -> raise (Error (Printf.sprintf "invalid ocaml id %S" (String.concat "" id)))

let parse_method id =
  let re = Re.char '#' |> Re.compile in
  match Re.split re id with
  | [id; mid] when not (is_capitalized id) && not (is_capitalized mid) -> (id, mid)
  | _ -> raise (Error (Printf.sprintf "invalid method name %S" id))

let parse_contents contents =
  match contents with
  | None | Some "" -> raise (Error "contents must be an Ocaml id")
  | Some def ->
    let def = Re.split seps def in
    match def with
    | "intro" :: [] -> [], `Index
    | "index" :: [] -> [], `Index
    | "index" :: "types" :: _ -> [], `IndexTypes
    | "index" :: "exceptions" :: _ -> [], `IndexExceptions
    | "index" :: "values" :: _ -> [], `IndexValues
    | "index" :: "attributes" :: _ -> [], `IndexAttributes
    | "index" :: "methods" :: _ -> [], `IndexMethods
    | "index" :: "classes" :: _ -> [], `IndexClasses
    | "index" :: "class" :: "types" :: _ -> [], `IndexClassTypes
    | "index" :: "modules" :: _ -> [], `IndexModules
    | "index" :: "module" :: "types" :: _ -> [], `IndexModuleTypes
    | "val":: lid | "value":: lid ->
      let path, id = parse_lid lid in
      path, `Value id
    | "type":: lid ->
      let path, id = parse_lid lid in
      path, `Type id
    | "class":: "type" :: lid ->
      let path, id = parse_lid lid in
      path , `ClassType id
    | "class":: lid ->
      let path, id = parse_lid lid in
      path , `Class id
    | "module":: "type" :: uid
    | "mod":: "type" :: uid ->
      let path, id = parse_uid uid in
      path, `ModType id
    | "module":: uid
    | "mod":: uid ->
      let path, id = parse_uid uid in
      path, `Mod id
    | "exception":: uid
    | "exc":: uid ->
      let path, id = parse_uid uid in
      path, `Exc id
    | "attribute":: lid
    | "attr":: lid ->
      let path, id = parse_lid lid in
      let id, did = parse_method id in
      path, `Attr (id, did)
    | "method":: lid ->
      let path, id = parse_lid lid in
      let id, mid = parse_method id in
      path, `Method (id, mid)
    | "section":: lid ->
      let path, id = parse_lid lid in
      path, `Section id
    | x :: _ -> raise (Error ("invalid contents: " ^ x))
    | [] -> raise (Error ("empty contents"))

(** OCaml identifier *)
type id =
  string list *
  [ `Mod of string    | `ModType of string
  | `Value of string  | `Type of string
  | `Class of string  | `ClassType of string
  | `Exc of string
  | `Method of (string * string) | `Attr of (string * string)
  | `Section of string
  | `Index
  | `IndexTypes
  | `IndexExceptions
  | `IndexValues
  | `IndexAttributes
  | `IndexMethods
  | `IndexClasses
  | `IndexClassTypes
  | `IndexModules
  | `IndexModuleTypes
  ]

let path_of_id ?(prefix) id =
  let add_prefix s =
    match prefix with
    | None -> s
    | Some p -> p ^ s
  in
  match id with
  | (path, `Index) -> add_prefix "index"
  | (path, `IndexTypes) -> add_prefix "index_types"
  | (path, `IndexExceptions) -> add_prefix "index_exceptions"
  | (path, `IndexValues) -> add_prefix "index_values"
  | (path, `IndexAttributes) -> add_prefix "index_attributes"
  | (path, `IndexMethods) -> add_prefix "index_methods"
  | (path, `IndexClasses) -> add_prefix "index_classes"
  | (path, `IndexClassTypes) -> add_prefix "index_class_types"
  | (path, `IndexModules) -> add_prefix "index_modules"
  | (path, `IndexModuleTypes) -> add_prefix "index_module_types"
  | (path, `ModType name)
  | (path, `Mod name) -> String.concat "." (path @ [name])
  | (path, `ClassType name)
  | (path, `Class name) ->
    begin match prefix with
    | None -> String.concat "." (path @ [name]) ^ "-c"
    | Some p -> p ^ (String.concat "." (path @ [name]))
    end
  | (path, `Attr (cl, _))
  | (path, `Method (cl, _)) ->
    begin match prefix with
    | None -> String.concat "." (path @ [cl]) ^ "-c"
    | Some p -> p ^ String.concat "." (path @ [cl])
    end
  | (path, `Value _)
  | (path, `Type _)
  | (path, `Exc _)
  | (path, `Section _) ->
    add_prefix (String.concat "." path)

let fragment_of_id : id -> string option = function
  | (_, `Value name) -> Some ("VAL" ^ name)
  | (_, `Type name) -> Some ("TYPE" ^ name)
  | (_, `Exc name) -> Some ("EXCEPTION" ^ name)
  | (_, `Attr (_, name)) -> Some ("ATTR" ^ name)
  | (_, `Method (_, name)) -> Some ("METHOD" ^ name)
  | (_, `Section name) -> Some name
  | _ -> None

let string_of_id ?(spacer = ".") : id -> string = function
  | (path, ( `Method (cl,name) | `Attr (cl, name))) ->
    name ^ " [" ^ String.concat spacer (path @ [cl]) ^"]"
  | (path, ( `Mod name   | `ModType name | `Class name | `ClassType name
           | `Value name | `Type name   | `Exc name )) ->
    String.concat spacer (path @ [name])
  | (_,`Index) -> "Api introduction"
  | (_, `IndexTypes)
  | (_, `IndexExceptions)
  | (_, `IndexValues)
  | (_, `IndexAttributes)
  | (_, `IndexMethods)
  | (_, `IndexClasses)
  | (_, `IndexClassTypes)
  | (_, `IndexModules)
  | (_, `IndexModuleTypes)
  | (_,`Section _) -> ""
