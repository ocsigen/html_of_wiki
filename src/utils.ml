module Operators = struct
  let (>>=) x f = match x with
    | Some x -> Some (f x)
    | None -> None

  let (|?) x default = match x with
    | Some x -> x
    | None -> default

  let (+/+) p q = Paths.(p +/+ q)
end


let id x = x

let zipk f g k = f (fun fk -> g (fun gk -> k fk gk))

let check_errors =
  List.iter (fun (err, b) -> if Lazy.force b then () else failwith err)

let trim char string =
  let rem_first s = String.sub s 1 (String.length s - 1) in
  let rec trim = function
    | "" -> ""
    | s when s.[0] = char -> trim @@ rem_first s
    | s -> s
  in
  trim string


let sorted_dir_files sort dir = Sys.readdir dir |> Array.to_list |> sort
let dir_files = sorted_dir_files id
let a'_sorted_dir_files = sorted_dir_files (List.sort compare)

(* FIXME use Lwt_unix.files_of_directory *)
let rec find_files name = function
  | file when Filename.basename file = name -> [file]
  | dir when Sys.is_directory dir ->
    dir_files dir
    |> List.map (fun f -> Paths.(dir +/+ f))
    |> List.map (find_files name)
    |> List.concat
  | _ -> []


let uri_absolute =
  let rex = Re.Pcre.regexp "^(http|https|file|localhost:\\d+)://" in
  Re.Pcre.pmatch ~rex


let read_in_channel ic =
  let rec readall () lines =
    try
      let line = input_line ic in
      readall () (line :: lines)
    with End_of_file -> List.rev lines
  in
  String.concat "\n" @@ readall () []

let readfile file =
  let ic = open_in file in
  let result = read_in_channel ic in
  close_in ic;
  result
