val up : string
val here : string

(** Operator for [Filename.concat]. *)
val (+/+) : string -> string -> string

(** Concatenates the paths together in one single path *)
val path_of_list : string list -> string

(** Splits the given path in a list of basenames *)
val list_of_path : string -> string list

(** Returns the absolute path of the given path (returns it if already absolute) *)
val realpath : string -> string

(** Checks whether two absolute path or two relative path are equal. *)
val path_eql : string -> string -> bool

(** [rewind dir file] returns the relative path to take to go back
    from file's directory to dir directory.
    Example: [rewind "foo/" "foo/bar/f.txt"] => "../../" *)
val rewind : string -> string -> string

(** [is_inside_dir dir file] returns whether [file] is located inside [dir]. *)
val is_inside_dir : string -> string -> bool

(** [remove_prefixl l l'] returns the l or l' with the prefix l' or l
    removed. *)
val remove_prefixl : 'a list -> 'a list -> 'a list

(** Does the same work thab remove_prefixl but for paths. *)
val path_rm_prefix : string -> string -> string

(** [is_visible d] returns whether [d] is a visible file (unix only). *)
val is_visible : string -> bool

(** [is_visible_dir d] returns whether [d] is a visible directory (unix only). *)
val is_visible_dir : string -> bool

(** [concat_uri_suffix s u] concatenates [u] with [s] if [u] doesn't ends
    with a ['/']. Raises [Failure] when [u] is [""]. *)
val concat_uri_suffix : string -> string -> string
