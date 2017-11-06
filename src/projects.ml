let wiki_dir = ref "." (* FIXME? *)

let is_directory path name =
  let full = Filename.concat path name in
  name.[0] <> '.' && name.[0] <> '_' && name.[0] <> '#' && Sys.is_directory full

let readdir path =
  let full = Filename.concat !wiki_dir path in
  Sys.readdir full |>
  Array.to_list |>
  List.filter (fun name -> is_directory full name)

let projects =
  readdir "" |>
  List.map (fun p ->
    readdir p |>
    List.map Version.parse |>
    List.sort (fun x y -> - Version.compare x y) |> function
    | [] ->
      Printf.eprintf "no versions found for %s...\n%!" p;
      []
    | Version.Dev :: latest :: _
    | latest :: _ ->
      [p, latest]
  ) |>
  List.flatten

let ids =
  projects |> List.map (fun (p, _) ->
    let full =
      let project_full = Filename.concat !wiki_dir p in
      Filename.concat project_full "config.js"
    in
    try
      Yojson.Safe.from_file full |>
      Yojson.Safe.Util.member "wiki_id" |>
      Yojson.Safe.Util.to_int |> fun id ->
      [id, p]
    with _ ->
      Printf.eprintf "couldn't get %s's wiki_id...\n%!" p;
      []
  ) |>
  List.flatten

let latest_of project =
  List.assoc project projects


(*
let () =
  Printf.printf "projects:\n";
  projects |> List.iter (fun (p, v) ->
    Printf.printf "%s: %s\n" p (Version.to_string v)
  );
  Printf.printf "ids:\n";
  ids |> List.iter @@ fun (id, p) ->
    Printf.printf "%d -> %s\n" id p
*)
