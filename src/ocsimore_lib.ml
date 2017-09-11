let spaces = Re.rep1 Re.blank |> Re.compile
let eols = Re.rep1 Re.eol |> Re.compile
let dot = Re.char '.' |> Re.compile

let get_opt args name =
  try Some (List.assoc name args)
  with Not_found -> None

let get_def ~default args name =
  try List.assoc name args
  with Not_found -> default

let get_project_version args =
  List.assoc "project" args,
  try Some (List.assoc "version" args) with Not_found -> None

module String = struct
  include String

  let sep char s =
    let len = String.length s in
    let seppos = String.index s char in
    String.trim (String.sub s 0 (seppos-1)),
    String.trim (String.sub s (seppos+1) (len-1))
end

(* FIXME maybe this is useless *)
let section = Lwt_log.Section.make "ocsimore"
