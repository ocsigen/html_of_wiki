module Operators = struct
  let (>>=) x f = match x with
    | Some x -> Some (f x)
    | None -> None

  let (|?) x default = match x with
    | Some x -> x
    | None -> default
end


let id x = x

let zipk f g k = f (fun fk -> g (fun gk -> k fk gk))


let trim char string =
  let rem_first s = String.sub s 1 (String.length s - 1) in
  let rec trim = function
    | "" -> ""
    | s when s.[0] = char -> trim @@ rem_first s
    | s -> s
  in
  trim string


let path_of_list = List.fold_left Filename.concat ""

let list_of_path p =
  let rec list_of_path = function
    | "." -> []
    | "/" | ".." as p -> [p]
    | p -> (Filename.basename p) :: list_of_path (Filename.dirname p)
  in
  list_of_path p |> List.rev

let realpath = function
  | p when Filename.is_relative p -> Filename.concat (Sys.getcwd ()) p
  | p -> p

let rec path_eql p p' = match (p, p') with
  | (".", ".") | ("/", "/") -> true
  | (".", _) | (_, ".") | ("/", _) | (_, "/") -> false
  | (_, _) -> path_eql (Filename.dirname p) (Filename.dirname p')

let rewind dir file =
  let dir = realpath dir in
  let rec rew = function
    | p when path_eql p dir -> "."
    | "." | "/" -> failwith @@ "rewind: " ^ file ^ " cannot be rewinded to dir " ^ dir
    | p -> Filename.concat ".." @@ rew @@ Filename.dirname p
  in
  file |> realpath |> Filename.dirname |> rew

let rec remove_prefixl l l' = match (l, l') with
  | (l, []) | ([], l) -> l
  | (x :: l, y :: l') when x = y -> remove_prefixl l l'
  | (_, _) -> failwith "remove_prefixl: no list is a prefix of the other"

let path_rm_prefix prefix p = (* works the other way round ;) *)
  remove_prefixl (list_of_path prefix) (list_of_path p) |> path_of_list


let uri_absolute =
  let rex = Re.Pcre.regexp "^(http|https|file|localhost:\\d+)://" in
  Re.Pcre.pmatch ~rex




let read_in_channel ic =
  let rec readall () =
    try
      let line = input_line ic in
      line :: readall ()
    with End_of_file -> []
  in
  String.concat "\n" @@ readall ()

let readfile file =
  let ic = open_in file in
  let result = read_in_channel ic in
  close_in ic;
  result
