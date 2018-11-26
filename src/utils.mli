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

  (** [l @< l'] returns whether every element in [l] is also
      inside [l'] (in terms of [=]). *)
  val (@<) : 'a list -> 'a list -> bool

  (** [l @- l'] returns the list of the elements of [l] not present
      inside [l'] (in terms of [=]). *)
  val (@-) : 'a list -> 'a list -> 'a list
end

(** The identity function. *)
val id : 'a -> 'a

(** [constantly x = fun _ -> x] *)
val constantly : 'a -> 'b -> 'a

(** Association lists type. *)
type ('a, 'b) alist = ('a * 'b) list

(** Continuation argument zipper.
    [f (fun a -> g (fun b -> ...))] = [zipk f g (fun a b -> ...)] *)
val zipk : (('a -> 'b) -> 'c) -> (('d -> 'e) -> 'b) -> ('a -> 'd -> 'e) -> 'c

(** [zip [a1;a2;...;aN] [b1;b2;...;bM]] returns
    [[(a1,b1); (a2,b2); ...; (aL,bL)]] with [L = min N M]. *)
val zip : 'a list -> 'b list -> ('a * 'b) list

(** Inverse of [zip]. *)
val unzip : ('a * 'b) list -> 'a list * 'b list

(** Groups association lists using [=] keys. For example,
    [group_alists [(1, 10); (2, 20); (3, 30)] [(2, 0); (3, 1); (4, 2)]] returns
    [[(2, (10, 0)); (3, (30, 1))]]. *)
val group_alists : ('a, 'b) alist -> ('a, 'c) alist -> ('a, ('b * 'c)) alist

(** [alist_of_values f [x; x'; ...]] returns [(x, f x); (x', f x'); ...]. *)
val alist_of_values : ('b -> 'a) -> 'b list -> ('a, 'b) alist

(** [check_errors [(msg, exp); ...]] evaluates in order each [exp] and raises
    [Failure msg] with the [msg] of the first [exp] to return [false], if any. *)
val check_errors : (string * bool lazy_t) list -> unit

(** Converts an hash table to an association list. *)
val alist_of_hashtbl : ('a, 'b) Hashtbl.t -> ('a * 'b) list

(** [is_some x] returns whether [x] is [Some y]. *)
val is_some : 'a option -> bool

(** [is_none x] returns whether [x] is [None]. *)
val is_none : 'a option -> bool

(** Inverse of [not_foundify]. *)
val optionify : ('a -> 'b) -> ('a -> 'b option)

(** [not_foundify f] returns a wrapper of [f] that raises [Not_found]
    when [f] returns [None]. *)
val not_foundify : ('a -> 'b option) -> ('a -> 'b)

(** [trim_n n s] returns [s] without its [n] first characters. *)
val trim_n : int -> string -> string

(** [trim c s] returns [s] with the trailing occurences of [c] removed. *)
val trim : char -> string -> string

(** [starts_with s s'] retruns whether [s] is a prefix of [s]. *)
val starts_with : string -> string -> bool

(** [ends_with s s'] retruns whether [s] is a suffix of [s]. *)
val ends_with : string -> string -> bool

(** Pretty prints the given columns with even horizontal spacing. *)
val sprint_two_cols : ?prefix:string -> ?sep:string -> (string * string) list -> string

(** Pretty prints the given columns with even horizontal spacing. *)
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
