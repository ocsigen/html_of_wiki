(** Defines general purpose operators. *)
module Operators : sig
  (** Bind operator for Maybe monad *)
  val (>>=) : 'a option -> ('a -> 'b) -> 'b option

  (** Elvis operator *)
  val (|?) : 'a option -> 'a -> 'a
end

(** The identity function. *)
val id : 'a -> 'a

(** Continuation argument zipper.
    [f (fun a -> g (fun b -> ...))] = [zipk f g (fun a b -> ...)] *)
val zipk : (('a -> 'b) -> 'c) -> (('d -> 'e) -> 'b) -> ('a -> 'd -> 'e) -> 'c

(** [trim c s] returns [s] with the trailing occurences of [c] removed. *)
val trim : char -> string -> string

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

(** [remove_prefixl l l'] returns the l or l' with the prefix l' or l
    removed. *)
val remove_prefixl : 'a list -> 'a list -> 'a list

(** Does the same work thab remove_prefixl but for paths. *)
val path_rm_prefix : string -> string -> string

(** [uri_absolute uri] returns whether [uri] is an absolute URI. *)
val uri_absolute : string -> bool

(** Reads all the content of ic and returns it as a string. *)
val read_in_channel : in_channel -> string

(** Read all the content of file and returns it as a string. *)
val readfile : string -> string
