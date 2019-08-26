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

val id : 'a -> 'a
(** The identity function. *)

val zipk :
  (('a -> 'b) -> 'c) -> (('d -> 'e) -> 'b) -> ('a -> 'd -> 'e) -> 'c
(** Continuation argument zipper.
    [f (fun a -> g (fun b -> ...))] = [zipk f g (fun a b -> ...)] *)

val check_errors : (string * bool lazy_t) list -> unit
(** [check_errors [(msg, exp); ...]] evaluates in order each [exp] and raises
    [Failure msg] with the [msg] of the first [exp] to return [false], if any. *)

val is_some : 'a option -> bool
(** [is_some x] returns whether [x] is [Some y]. *)

val is_none : 'a option -> bool
(** [is_none x] returns whether [x] is [None]. *)

val trim : char -> string -> string
(** [trim c s] returns [s] with the trailing occurences of [c] removed. *)

val sorted_dir_files :
  (string list -> string list) -> string -> string list
(** [sorted_dir_files sort dir] returns the list of the files inside [dir]
    sorted using the given [sort] function. *)

val dir_files : string -> string list
(** [dir_files dir] retuns the list of the files inside dir in the order
    [readdir] returns them (i.e: no order guaranteed). *)

val a'_sorted_dir_files : string -> string list
(** [a'_sorted_dir_files dir] returns the list of the files inside [dir]
    (alpha)betically sorted. *)

val find_files : string -> string -> string list
(** [find_files name dir] returns the paths to all the files with [name]
    recursively found inside [dir]. *)

val uri_absolute : string -> bool
(** [uri_absolute uri] returns whether [uri] is an absolute URI. *)

val read_channel_lines : in_channel -> string list
(** Reads and returns the lines of the given [in_channel]. *)

val read_in_channel : in_channel -> string
(** Reads all the content of ic and returns it as a string. *)

val read_file_lines : string -> string list
(** Reads and returns the lines inside the given file. *)

val read_file : string -> string
(** Read all the content of file and returns it as a string. *)
