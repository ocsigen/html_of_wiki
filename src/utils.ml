module Operators = struct
  let (>>=) x f = match x with
    | Some x -> f x
    | None -> None

  let (<$>) x f = match x with
    | Some x -> Some (f x)
    | None -> None

  let (|?) x default = match x with
    | Some x -> x
    | None -> default

  let (+/+) p q = Paths.(p +/+ q)

  let rec (^*) s = function
    | 0 -> ""
    | n when n > 0 -> s ^ (s ^* (n - 1))
    | _ -> failwith "string multiplication operator: negative operand"
end


let id x = x

let zipk f g k = f (fun fk -> g (fun gk -> k fk gk))

let check_errors =
  List.iter (fun (err, b) -> if Lazy.force b then () else failwith err)

let is_some = function Some _ -> true | None -> false
let is_none = function Some _ -> false | None -> true

let optionify f = fun x -> match f x with
  | exception Not_found -> None
  | x -> Some x
let not_foundify f = fun x -> match f x with
  | Some x -> x
  | None -> raise Not_found


let trim_n n string = match String.length string with
  | len when len <= n -> ""
  | len -> String.sub string n (len - n)

let trim char string =
  let rem_first = trim_n 1 in
  let rec trim = function
    | "" -> ""
    | s when s.[0] = char -> trim @@ rem_first s
    | s -> s
  in
  trim string

let starts_with prefix s =
  let p = String.length prefix in
  String.length s >= p && String.sub s 0 p = prefix
let ends_with suffix s =
  let l = String.length suffix in
  String.length s >= l && String.sub s (String.length s - l) l = suffix

let sprint_two_cols ?(prefix = "") ?(sep = "\t ") cols =
  let max_len = cols
                |> List.map (fun (fstcol, _) -> String.length fstcol)
                |> List.fold_left max 0
  in
  cols
  |> List.map (fun (c, c') ->
      Printf.sprintf "%s%s%s\t %s"
        prefix c
        Operators.(" " ^* (max_len - String.length c)) c')
  |> String.concat "\n"



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


let read_channel_lines ic =
  let rec readall lines =
    try
      let line = input_line ic in
      readall (line :: lines)
    with End_of_file -> List.rev lines
  in
  readall []

let read_file_lines file =
  let ic = open_in file in
  let lines = read_channel_lines ic in
  close_in ic;
  lines

let read_in_channel ic = read_channel_lines ic |> String.concat "\n"
let read_file file = read_file_lines file |> String.concat "\n"
