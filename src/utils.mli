(** Defines general purpose operators. *)
module Operators : sig
  (** Bind operator for the Maybe monad *)
  val (>>=) : 'a option -> ('a -> 'b option) -> 'b option

  (** Map operator for the Maybe monad *)
  val (<$>) : 'a option -> ('a -> 'b) -> 'b option

  (** Elvis operator *)
  val (|?) : 'a option -> 'a -> 'a

  (** Path concatenation operator. See module [Paths]. *)
  val (+/+) : string -> string -> string

  (** [s ^* n] equals to [s ^ s ^ ... ^ s], [n] times. *)
  val (^*) : string -> int -> string

  val (@<) : 'a list -> 'a list -> bool

  val (@-) : 'a list -> 'a list -> 'a list
end

(** The identity function. *)
val id : 'a -> 'a

val constantly : 'a -> 'b -> 'a

type ('a, 'b) alist = ('a * 'b) list

(** Continuation argument zipper.
    [f (fun a -> g (fun b -> ...))] = [zipk f g (fun a b -> ...)] *)
val zipk : (('a -> 'b) -> 'c) -> (('d -> 'e) -> 'b) -> ('a -> 'd -> 'e) -> 'c

val zip : 'a list -> 'b list -> ('a * 'b) list

val unzip : ('a * 'b) list -> 'a list * 'b list

val group_alists : ('a, 'b) alist -> ('a, 'c) alist -> ('a, ('b * 'c)) alist

val alist_of_values : ('b -> 'a) -> 'b list -> ('a, 'b) alist

(** [check_errors [(msg, exp); ...]] evaluates in order each [exp] and raises
    [Failure msg] with the [msg] of the first [exp] to return [false], if any. *)
val check_errors : (string * bool lazy_t) list -> unit

val alist_of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(** [is_some x] returns whether [x] is [Some y]. *)
val is_some : 'a option -> bool

(** [is_none x] returns whether [x] is [None]. *)
val is_none : 'a option -> bool

val optionify : ('a -> 'b) -> ('a -> 'b option)

val not_foundify : ('a -> 'b option) -> ('a -> 'b)

val trim_n : int -> string -> string

(** [trim c s] returns [s] with the trailing occurences of [c] removed. *)
val trim : char -> string -> string

val starts_with : string -> string -> bool

val ends_with : string -> string -> bool

val sprint_two_cols : ?prefix:string -> ?sep:string -> (string * string) list -> string

val sprint_three_cols : ?prefix:string -> ?sep:string -> (string * string * string) list -> string

(** [sorted_dir_files sort dir] returns the list of the files inside [dir]
    sorted using the given [sort] function. *)
val sorted_dir_files : (string list -> string list) -> string -> string list

(** [dir_files dir] retuns the list of the files inside dir in the order
    [readdir] returns them (i.e: no order guaranteed). *)
val dir_files : string -> string list

(** [a'_sorted_dir_files dir] returns the list of the files inside [dir]
    (alpha)betically sorted. *)
val a'_sorted_dir_files : string -> string list

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
