let up = ".."
let here = "."
let ( +/+ ) = Filename.concat
let path_of_list = List.fold_left Filename.concat ""

let list_of_path p =
  let rec list_of_path = function
    | "." -> []
    | "" -> []
    | ("/" | "..") as p -> [ p ]
    | p -> Filename.basename p :: list_of_path (Filename.dirname p)
  in
  let rev_path = list_of_path p in
  let rev_path = if p <> "" && String.get p (String.length p - 1) = '/' then "" :: rev_path else rev_path in
  List.rev rev_path

let realpath = function
  | p when Filename.is_relative p -> Filename.concat (Sys.getcwd ()) p
  | p -> p

let rec path_eql p p' =
  match (p, p') with
  | ".", "." | "/", "/" -> true
  | ".", _ | _, "." | "/", _ | _, "/" -> false
  | _, _ when Filename.(basename p <> basename p') -> false
  | _, _ -> path_eql (Filename.dirname p) (Filename.dirname p')

let rewind dir file =
  let dir = realpath dir in
  let rec rew = function
    | p when path_eql p dir -> "."
    | "." | "/" ->
      failwith @@ "rewind: " ^ file ^ " cannot be rewinded to dir " ^ dir
    | p -> Filename.concat ".." @@ rew @@ Filename.dirname p
  in
  file |> realpath |> Filename.dirname |> rew

let is_inside_dir dir file =
  match rewind dir file with
  | _ -> true
  | exception Failure _ -> false

let rec remove_prefixl l l' =
  match (l, l') with
  | l, [] | [], l -> l
  | x :: l, y :: l' when x = y -> remove_prefixl l l'
  | _, _ -> failwith "remove_prefixl: no list is a prefix of the other"

let path_rm_prefix prefix p =
  (* works the other way round ;) *)
  try remove_prefixl (list_of_path prefix) (list_of_path p) |> path_of_list
  with _e ->
    failwith
      (Printf.sprintf
         "remove_prefixl: no list is a prefix of the other, %s is not a prefix \
          of %s"
         prefix p)

let is_visible = function
  | "" -> false
  | f -> f.[0] <> '.'

let is_visible_dir d = Sys.is_directory d && is_visible d

let concat_uri_suffix suffix = function
  | "" -> failwith "concat_uri_suffix: empty uri"
  | uri when uri.[String.length uri - 1] = '/' -> uri
  | uri -> uri ^ suffix

let apply_path =
  let rec loop n path =
    match Filename.basename path with
    | "/" -> "/"
    | "." when path = "" || path = "." -> ""
    | "" | "." -> loop n (Filename.dirname path)
    | ".." -> loop (n + 1) (Filename.dirname path)
    | _ when n > 0 -> loop (n - 1) (Filename.dirname path)
    | b -> loop n (Filename.dirname path) +/+ b
  in
  loop 0
