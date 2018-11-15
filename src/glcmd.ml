open Utils.Operators

type doc = string

type argument_name = Short of string | Long of string
type argument_value = string
type argument_type =
  | File
  | String
  | Choice of argument_value list
type argument_names = argument_name list
type argument_valname = string option
type 'a argument =
  | Flag of argument_names * 'a * doc
  | Arg of argument_names * argument_type * 'a * argument_valname * doc
  | Required of argument_names * argument_type * 'a * argument_valname * doc
  | Positional of argument_type * argument_valname * doc
  | Multiple of argument_type * argument_valname * doc

type command_name = string
type 'a command_args = 'a argument list
type ('a, 'b) command_fun = ('a * argument_value) list -> argument_value list -> 'b
type ('a, 'b) command =
  | Prefix of command_name * ('a, 'b) command list
  | Command of command_name * 'a command_args * doc * ('a, 'b) command_fun
  | Anonymous of 'a command_args * doc * ('a, 'b) command_fun
type ('a, 'b) commandline = ('a, 'b) command list

exception Empty_commandline
exception Unknown_command of string
exception Incomplete_command
exception Unknown_argument of string
exception Missing_arguments of string list
exception Invalid_argument_value
exception Missing_argument_value


let is_anonymous                = function Anonymous _ -> true | _ -> false
let is_command                  = function Command _ -> true | _ -> false
let is_prefix                   = function Prefix _ -> true | _ -> false
let is_flag                     = function Flag _ -> true | _ -> false
let is_arg                      = function Arg _ -> true | _ -> false
let is_required                 = function Required _ -> true | _ -> false
let is_positional               = function Positional _ -> true | _ -> false
let is_multiple                 = function Multiple _ -> true | _ -> false
let is_required_or_positional x = is_required x || is_positional x
let is_positional_or_multiple x = is_positional x || is_multiple x
let is_named x                  = not @@ is_positional_or_multiple x
let is_short_name               = function Short _ -> true | _ -> false
let is_long_name                = function Long _ -> true | _ -> false


let has_anonymous cmdline = List.exists is_anonymous cmdline
let find_anonymous cmdline = List.find is_anonymous cmdline

let rec collect_cmd_names = function
  | [] -> []
  | Prefix (n, _) :: cs | Command (n, _, _, _) :: cs -> n :: collect_cmd_names cs
  | _ :: cs -> collect_cmd_names cs

let rec find_command name = function
  | [] -> raise Not_found
  | (Prefix (n, _) as c) :: _ | (Command (n, _, _, _) as c) :: _ when n = name -> c
  | _ :: cs -> find_command name cs

let subcmdline name cmdline = match find_command name cmdline with
  | Prefix (_, cs) -> cs
  | _ -> raise Not_found

let rec find_tree cmdline cns =
  let extract_tree = function
    | Prefix (_, t) -> t
    | x -> [x]
  in
  let find cn = find_command cn cmdline |> extract_tree in
  match cns with
  | [] -> cmdline
  | [cn] -> find cn
  | cn :: cns -> find_tree (find cn) cns

let first_name = function
  | [] -> raise Not_found
  | ns -> List.find_opt is_long_name ns |? List.hd ns

let sort_arguments arguments =
  List.filter is_named arguments @ List.filter is_positional arguments @ (begin
      match List.filter is_multiple arguments with
      | ([] as l) | ([_] as l) -> l
      | _ -> assert false (* multiple Multiple arguments TODO check *)
    end)


let sprint_argument_name = function
  | Short n -> "-" ^ n
  | Long n -> "--" ^ n

let sprint_first_name ns = first_name ns |> sprint_argument_name

let sprint_argument_type = function
  | File -> "FILE"
  | String -> "VAL"
  | Choice cs -> Printf.sprintf "<%s>" (String.concat "|" cs)

let sprint_argument = function
  | Flag (ns, _, _) -> Printf.sprintf "[%s]" (sprint_first_name ns)
  | Arg (ns, _, _, Some vn, _) -> Printf.sprintf "[%s %s]" (sprint_first_name ns) vn
  | Arg (ns, t, _, None, _) -> Printf.sprintf "[%s %s]" (sprint_first_name ns) (sprint_argument_type t)
  | Required (ns, _, _, Some vn, _) -> Printf.sprintf "%s %s" (sprint_first_name ns) vn
  | Required (ns, t, _, None, _) -> Printf.sprintf "%s %s" (sprint_first_name ns) (sprint_argument_type t)
  | Positional (t, vn, _) -> Printf.sprintf "%s" (vn |? sprint_argument_type t)
  | Multiple (t, vn, _) ->  Printf.sprintf "[%s...]" (vn |? sprint_argument_type t)

let sprint_arguments arguments = String.concat " " (List.map sprint_argument arguments)
let print_arguments out arguments = Printf.fprintf out "%s" (sprint_arguments arguments)

let sprint_category name = Printf.sprintf "%s\n%s\n" (String.uppercase_ascii name)

let describe_arguments args =
  let names ns = List.map sprint_argument_name ns |> String.concat ", " in
  let names_vn ns vn =
    let v = "=" ^ vn in
    (List.map sprint_argument_name ns
     |> String.concat (v ^ ", ")) ^ v
  in
  let argument_data = function
    | Flag (ns, _, doc) -> names ns, doc
    | Arg (ns, _, _, Some vn, doc) | Required (ns, _, _, Some vn, doc) ->
      names_vn ns vn, doc
    | Arg (ns, t, _, None, doc) | Required (ns, t, _, None, doc) ->
      names_vn ns (sprint_argument_type t), doc
    | Positional (t, vn, doc) -> (vn |? sprint_argument_type t), doc
    | Multiple (t, vn, doc) -> (vn |? sprint_argument_type t) ^ "...", doc
  in
  let collect_data p = List.filter p args |> List.map argument_data in
  let named = collect_data is_named in
  let positional = collect_data is_positional @ collect_data is_multiple in
  let sprint_data header = function
    | [] -> ""
    | data -> sprint_category header (Utils.sprint_two_cols ~prefix:"\t" data)
  in
  sprint_data "named arguments" named ^ sprint_data "positional arguments" positional

let describe_tree prefix tree =
  let no_data = ("", "") in
  let rec command_data prefix = function
    | Anonymous (args, doc, _) -> Printf.sprintf "%s%s" prefix (sprint_arguments args), doc
    | Command (name, args, doc, _) -> Printf.sprintf "%s%s %s" prefix name (sprint_arguments args), doc
    | Prefix (name, st) -> match find_anonymous st with
      | exception Not_found -> no_data
      | an -> command_data (prefix ^ name) an
  in
  let data = tree
             |> List.map (command_data prefix)
             |> List.filter (fun x -> x <> no_data)
  in
  match tree with
  | [Command (_, args, _, _)] when data <> [] ->
    let desc, usage =
      let usg, doc = List.hd data in
      [(doc, "")], [(usg, "")]
    in
    sprint_category "description" ("\t" ^ Utils.sprint_two_cols desc)
    ^ sprint_category "usage" ("\t" ^ Utils.sprint_two_cols usage)
    ^ (describe_arguments args)
  | _ -> Utils.sprint_two_cols data


let help ?prefix cmd_path cmdline =
  let rec not_last = function [] | [_] -> [] | x :: xs -> x :: not_last xs in
  let tree = find_tree cmdline cmd_path in
  let pre_path = match tree with
    | [Command _] -> not_last cmd_path
    | _ -> cmd_path
  in
  let s = String.concat " " pre_path in
  describe_tree ((prefix <$> (fun p -> p ^ s) |? s) ^ " ") tree


let first_name_string ns = match first_name ns with Short n | Long n -> n

let first_arg_name = function
  | Flag (ns, _, _) | Arg (ns, _, _, _, _) | Required (ns, _, _, _, _) ->
    first_name_string ns
  | Positional (t, vn, _) | Multiple (t, vn, _) -> vn |? sprint_argument_type t

(* The [args] is required not to be inferred with a weak type. *)
let required_arguments args = List.filter is_required_or_positional args

let required_arguments_names args = print_arguments stdout @@ required_arguments args; required_arguments args |> List.map first_arg_name


let arg_names_match ns = function
  | n when Utils.starts_with "--" n -> List.mem (Long (Utils.trim_n 2 n)) ns
  | n when Utils.starts_with "-" n -> List.mem (Short (Utils.trim_n 1 n)) ns
  | _ -> false

let is_argument name = function
  | Flag (ns, _, _) | Arg (ns, _, _, _, _) | Required (ns, _, _, _, _) ->
    arg_names_match ns name
  | _ -> false

let find_argument name = List.find (is_argument name)


let rec find_positional_argument = function
  | [] -> raise Not_found
  | (Positional _ as arg) :: _ | (Multiple _ as arg) :: _ -> arg
  | _ :: cs -> find_positional_argument cs

let validate_value t value = match t with
  | File when Sys.file_exists value -> ()
  | Choice choices when List.mem value choices -> ()
  | String -> ()
  | File -> raise Invalid_argument_value
  | Choice _ -> raise Invalid_argument_value

let parse_argument arg argv =
  let type_value t = match argv with
      | value :: argv ->
        validate_value t value;
        (value, argv)
      | [] -> raise Missing_argument_value
  in
  match arg with
  | Flag (ns, id, _) -> (`Named (id, first_name_string ns), argv)
  | Arg (_, t, id, _, _) | Required (_, t, id, _, _) ->
    let value, argv = type_value t in
    (`Named (id, value), argv)
  | Positional (t, _, _) ->
    let value, argv = type_value t in
    (`Pos value, argv)
  | Multiple (t, _,  _) ->
    let validate value =
      validate_value t value;
      value
    in
    (`Mul (List.map validate argv), [])

let parse_arguments =
  let remove x = List.filter (fun y -> x <> y) in
  let find_argument n args = (Utils.optionify @@ find_argument n) args in
  let find_positional_argument arg = Utils.optionify find_positional_argument arg in
  let rec parse args = function
    | [] when required_arguments args = [] -> ([], [])
    | a :: argv ->
      let collect_value () = match (find_argument a args, find_positional_argument args) with
        | (Some arg, _) -> arg, parse_argument arg argv
        | (None, Some arg) -> arg, parse_argument arg (a :: argv) (* if it's positional its a value *)
        | (None, None) -> raise (Unknown_argument a)
      in
      let arg, (value, argv) = collect_value () in
      let (named, positional) = parse (remove arg args) argv in
      begin match value with
        | `Named x -> (x :: named, positional)
        | `Pos x -> (named, x :: positional)
        | `Mul xs -> (named, xs @ positional)
      end
    | [] -> raise (Missing_arguments (required_arguments_names args))
  in
  parse

let apply_cmdfun f arguments argv =
  let arguments = sort_arguments arguments in
  let (named, positional) = parse_arguments arguments argv in
  f named positional

let rec apply_anonymous argv cmdline = match find_anonymous cmdline with
  | Anonymous (args, _, f) -> apply_cmdfun f args argv
  | _ -> assert false

let rec parse_commands cmdline =
  let cmds_names = collect_cmd_names cmdline in
  function
  | name :: argv when List.mem name cmds_names ->
    begin match find_command name cmdline with
      | Prefix (_, cs) -> parse_commands cs argv
      | Command (_, args, _, f) -> apply_cmdfun f args argv
      | _ -> assert false
    end
  | args when has_anonymous cmdline -> apply_anonymous args cmdline
  | name :: _ -> raise (Unknown_command name)
  | [] -> raise Incomplete_command


let validate_commandline = function _ -> () (* TODO *)

let run = function
  | [] -> raise Empty_commandline
  | cmdline ->
    validate_commandline cmdline;
    Sys.argv
    |> Array.to_list
    |> List.tl (* skip argv[0] *)
    |> parse_commands cmdline


let anonymous ?(args = []) ?(doc = "") f = Anonymous (args, doc, f)
let command ?(args = []) ?(doc = "") n f = Command (n, args, doc, f)
let prefix name commands = Prefix (name, commands)


let names_of short long = (short <$> (fun s -> [Short s]) |? []) @ [Long long]
let flag ?short ?(doc = "") n x = Flag (names_of short n, x, doc)
let arg ?short ?default ?valname ?(doc = "") n t x = Arg (names_of short n, t, x, valname, doc)
let positional ?valname ?(doc = "") t = Positional (t, valname, doc)
let multiple ?valname ?(doc = "") t = Multiple (t, valname, doc)
