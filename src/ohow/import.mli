module Option : sig
  val bind : f:('a -> 'b option) -> 'a option -> 'b option
  (** Bind operator for the Maybe monad *)

  val map : f:('a -> 'b) -> 'a option -> 'b option
  (** Map operator for the Maybe monad *)

  val value : 'a option -> default:'a -> 'a

  val is_some : 'a option -> bool
  (** [is_some x] returns whether [x] is [Some y]. *)

  val is_none : 'a option -> bool
  (** [is_none x] returns whether [x] is [None]. *)
end

module String : sig
  include module type of struct
    include String
  end

  val cut : char -> string -> (string * string) option
  val split_on_blank : string -> string list

  val remove_leading : char -> string -> string
  (** [remove_leading c s] returns [s] with the trailing occurences of [c]
      removed. *)
end

module List : sig
  include module type of struct
    include List
  end

  module Assoc : sig
    type nonrec 'a t = (string * 'a) t

    val get_opt : 'a t -> string -> 'a option
  end
end

(** Defines general purpose operators. *)
module Operators : sig
  val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
  (** Bind operator for the Maybe monad *)

  val ( <$> ) : 'a option -> ('a -> 'b) -> 'b option
  (** Map operator for the Maybe monad *)

  val ( |? ) : 'a option -> 'a -> 'a
  (** Elvis operator *)

  val ( +/+ ) : string -> string -> string
  (** Path concatenation operator. See module [Paths]. *)
end

val find_files : string -> string -> string list
(** [find_files name dir] returns the paths to all the files with [name]
    recursively found inside [dir]. *)

val uri_absolute : string -> bool
(** [uri_absolute uri] returns whether [uri] is an absolute URI. *)

val read_file_lines : string -> string list
(** Reads and returns the lines inside the given file. *)

val read_file : string -> string
(** Read all the content of file and returns it as a string. *)
