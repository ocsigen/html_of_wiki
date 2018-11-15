(* Git-like command line argument parser *)

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

val find_tree : ('a, 'b) commandline -> command_name list -> ('a, 'b) commandline

val help : ?prefix:string -> string list -> ('a, 'b) commandline -> string

val run : ('a, 'b) commandline -> 'b


val anonymous : ?args:'a command_args -> ?doc:doc ->
  ('a, 'b) command_fun -> ('a, 'b) command
val command : ?args:'a command_args -> ?doc:doc ->
  command_name -> ('a, 'b) command_fun -> ('a, 'b) command
val prefix : command_name -> ('a, 'b) command list -> ('a, 'b) command

val flag : ?short:string -> ?doc:doc -> string -> 'a -> 'a argument
val arg :
  ?short:string -> ?default:argument_value ->
  ?valname:string -> ?doc:doc ->
  string -> argument_type -> 'a -> 'a argument
val positional : ?valname:string -> ?doc:doc -> argument_type -> 'a argument
val multiple : ?valname:string -> ?doc:doc -> argument_type -> 'a argument
