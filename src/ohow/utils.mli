(** Defines general purpose operators. *)
module Operators : sig
  (** Bind operator for the Maybe monad *)
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

  (** Map operator for the Maybe monad *)
  val ( <$> ) : 'a option -> ('a -> 'b) -> 'b option

  (** Elvis operator *)
  val ( |? ) : 'a option -> 'a -> 'a

  (** Path concatenation operator. See module [Paths]. *)
  val ( +/+ ) : string -> string -> string
end

(** [is_some x] returns whether [x] is [Some y]. *)
val is_some : 'a option -> bool

(** [is_none x] returns whether [x] is [None]. *)
val is_none : 'a option -> bool

(** [trim c s] returns [s] with the trailing occurences of [c] removed. *)
val trim : char -> string -> string

(** [find_files name dir] returns the paths to all the files with [name]
    recursively found inside [dir]. *)
val find_files : string -> string -> string list

(** [uri_absolute uri] returns whether [uri] is an absolute URI. *)
val uri_absolute : string -> bool

(** Reads and returns the lines of the given [in_channel]. *)
val read_channel_lines : in_channel -> string list

(** Reads all the content of ic and returns it as a string. *)
val read_in_channel : in_channel -> string

(** Reads and returns the lines inside the given file. *)
val read_file_lines : string -> string list

(** Read all the content of file and returns it as a string. *)
val read_file : string -> string
