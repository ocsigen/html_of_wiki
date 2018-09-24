module Operators = struct
  let (>>=) x f = match x with
    | Some x -> Some (f x)
    | None -> None

  let (|?) x default = match x with
    | Some x -> x
    | None -> default
end

let path_of_list = List.fold_left Filename.concat ""

let rec list_of_path = function
  | "/" | "." as p -> [p]
  | p -> (Filename.basename p) :: list_of_path (Filename.dirname p)

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
    | p when path_eql p dir -> ""
    | "." | "/" -> failwith @@ "rewind: " ^ file ^ " cannot be rewinded to dir " ^ dir
    | p -> Filename.concat ".." @@ rew @@ Filename.dirname p
  in
  file |> realpath |> Filename.dirname |> rew

let rec remove_prefixl l l' = match (l, l') with
  | (l, []) | ([], l) -> l
  | (x :: l, y :: l') when x = y -> remove_prefixl l l'
  | (_, _) -> failwith "remove_prefixl: no list is a prefix of the other"

let path_rm_prefix prefix p = (* works the other way round ;) *)
  let revlop p = list_of_path p |> List.rev in
  remove_prefixl (revlop prefix) (revlop p) |> List.rev |> path_of_list
