(** [with_current_file f k] sets the [current_file] to [f] and calls [k ()].
    Unsets the [current_file] after [k] finishes and returns its value. *)
val with_current_file : string -> (unit -> 'a) -> 'a
(** [using_current_file k] calls [k @@ current_file ()] and returns its value. *)
val using_current_file : (string -> 'a) -> 'a
(** Returns the current file path. *)
val current_file : unit -> string

(** Denotes the menu(.wiki) file currently being compiled. *)
type menu_file = Manual of string | Api of string
(** [with_menu_file mf k] sets the [menu_file] fo [mf] and calls [k ()].
    Unsets the [menu_file] after [k] finishes and returns its value. *)
val with_menu_file : menu_file -> (unit -> 'a) -> 'a
(** [using_menu_file k] calls [k @@ menu_file ()] and returns its value. *)
val using_menu_file : (menu_file -> 'a) -> 'a option
(** Returns the currently set [menu_file] or an error if none. *)
val menu_file : unit -> menu_file option
(** Returns [Some (menu_file ())] if [menu_file ()] is [Manual f] or [None] otherwise. *)
val manual_menu_file : unit -> string option
(** Returns [Some (menu_file ())] if [menu_file ()] is [Api f] or [None] otherwise. *)
val api_menu_file : unit -> string option

(** A type containing the values of the CLI options accepted by ohow. *)
type cli_options = {
  files: string list;
  print: bool;
  outfile: string option;
  root: string;
  manual: string;
  api: string;
  images: string;
  assets: string;
}

(** [with_options opts k] sets the [options] fo [opts] and calls [k ()].
    Unsets the [options] after [k] finishes and returns its value. *)
val with_options : cli_options -> (unit -> 'a) -> 'a
(** [using_options k] calls [k @@ options ()] and returns its value. *)
val using_options : (cli_options -> 'a) -> 'a
(** Returns the more recently set [cli_options]. *)
val options : unit -> cli_options

(** Returns [(options ()).root]. *)
val root   : unit -> string
(** Returns [(options ()).manual]. *)
val manual : unit -> string
(** Returns [(options ()).api]. *)
val api    : unit -> string
(** Returns [(options ()).images]. *)
val images : unit -> string
(** Returns [(options ()).assets]. *)
val assets : unit -> string

(** Alias for [root ()]. *)
val version_dir : unit -> string
(** Returns the absolute path to the project directory
    ([root]'s parent directory). *)
val project_dir : unit -> string
(** Returns the absolute path to the directory containing all projects
    ([project_dir]'s parent directory). *)
val all_projects_dir : unit -> string
