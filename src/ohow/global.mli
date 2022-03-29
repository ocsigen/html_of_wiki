(** [with_current_file f k] sets the [current_file] to [f] and calls [k ()].
    Unsets the [current_file] after [k] finishes and returns its value. *)
val with_current_file : string -> (unit -> 'a) -> 'a

(** [using_current_file k] calls [k @@ current_file ()] and returns its value. *)
val using_current_file : (string -> 'a) -> 'a

(** Returns the current file path. *)
val current_file : unit -> string

(** Denotes the menu(.wiki) file currently being compiled. *)
type menu_file =
  | Manual of string
  | Api of string

(** [with_menu_file mf k] sets the [menu_file] fo [mf] and calls [k ()]. Unsets
    the [menu_file] after [k] finishes and returns its value. *)
val with_menu_file : menu_file -> (unit -> 'a) -> 'a

(** [using_menu_file k] calls [k @@ menu_file ()] and returns its value. *)
val using_menu_file : (menu_file -> 'a) -> 'a option

(** Returns the currently set [menu_file] or an error if none. *)
val menu_file : unit -> menu_file option

(** Returns [Some (menu_file ())] if [menu_file ()] is [Manual f] or [None]
    otherwise. *)
val manual_menu_file : unit -> string option

(** Returns [Some (menu_file ())] if [menu_file ()] is [Api f] or [None]
    otherwise. *)
val api_menu_file : unit -> string option

(** A type containing the values of the CLI options accepted by ohow. *)
type cli_options =
  { files : string list
  ; print : bool
  ; pretty : bool
  ; headless : bool
  ; local : bool
  ; outfile : string option
  ; suffix : string
  ; project : string option
  ; root : string
  ; manual : string option
  ; api : string option
  ; default_subproject : string option
  ; images : string option
  ; assets : string option
  ; template : string option
  ; csw : string list
  ; docversions : string list
  }

(** [with_options opts k] sets the [options] fo [opts] and calls [k ()]. Unsets
    the [options] after [k] finishes and returns its value. *)
val with_options : cli_options -> (unit -> 'a) -> 'a

(** [using_options k] calls [k @@ options ()] and returns its value. *)
val using_options : (cli_options -> 'a) -> 'a

(** Returns the more recently set [cli_options]. *)
val options : unit -> cli_options

(** Returns [(options ()).suffix]. *)
val suffix : unit -> string

(** Returns the value of [(options ()).manual] is any, or raises [Failure]. *)
val the_manual : unit -> string

(** Returns the value of [(options ()).api] is any, or raises [Failure]. *)
val the_api : unit -> string

(** Returns the value of [(options ()).images] is any, or raises [Failure]. *)
val the_images : unit -> string

(** Returns the value of [(options ()).assets] is any, or raises [Failure]. *)
val the_assets : unit -> string

(** Returns [(options ()).root]. *)
val root : unit -> string

(** Alias for [root ()]. *)
val version_dir : unit -> string

(** Returns the absolute path to the project directory ([root]'s parent
    directory). *)
val project_dir : unit -> string

(** Returns the absolute path to the directory containing all projects
    ([project_dir]'s parent directory). *)
val all_projects_dir : unit -> string

(** The path to take from the project's root to end up in the website root
    (i.e., where the links [\[site:x\]] starts to). Defaults to [\[""\]]---the
    project's root is the website root. *)
val root_to_site : string ref
