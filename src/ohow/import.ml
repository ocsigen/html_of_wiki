module Option = struct
  include Option

  let bind x f =
    match x with
    | Some x -> f x
    | None -> None

  let map x f =
    match x with
    | Some x -> Some (f x)
    | None -> None

  let value x ~default =
    match x with
    | Some x -> x
    | None -> default

  let is_some = function
    | Some _ -> true
    | None -> false

  let is_none = function
    | Some _ -> false
    | None -> true
end

module String = struct
  include String

  let sep char s =
    let len = String.length s in
    try
      let seppos = String.index s char in
      Some
        ( String.trim (String.sub s 0 seppos)
        , String.trim (String.sub s (seppos + 1) (len - seppos - 1)) )
    with Not_found -> None

  let spaces = Re.rep1 Re.blank |> Re.compile
  let split_on_blank s = Re.split spaces s

  let remove_leading char s =
    let rec loop p =
      if p >= String.length s
      then ""
      else if String.get s p = char
      then loop (succ p)
      else String.sub s p (String.length s - p)
    in
    loop 0
end

module List = struct
  include List

  module Assoc = struct
    type 'a t = (string * 'a) list

    let get_opt args name =
      try Some (List.assoc name args) with Not_found -> None
  end
end

module Operators = struct
  let ( >>= ) = Option.bind
  let ( <$> ) = Option.map
  let ( |? ) x default = Option.value x ~default
  let ( +/+ ) p q = Paths.(p +/+ q)
end

let sorted_dir_files sort dir = Sys.readdir dir |> Array.to_list |> sort
let dir_files = sorted_dir_files (fun x -> x)

let rec find_files name = function
  | file when Filename.basename file = name -> [ file ]
  | dir when try Sys.is_directory dir with Sys_error _ -> false ->
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

let read_file file = read_file_lines file |> String.concat "\n"
