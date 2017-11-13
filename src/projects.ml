let is_directory path name =
  let full = Filename.concat path name in
  name.[0] <> '.' && name.[0] <> '_' && name.[0] <> '#' && Sys.is_directory full

let readdir wiki_dir path =
  let full = Filename.concat wiki_dir path in
  Sys.readdir full |>
  Array.to_list |>
  List.filter (fun name -> is_directory full name)

let projects = ref []
let ids = ref []

let init wiki_dir =
  projects :=
    readdir wiki_dir "" |>
    List.map (fun p ->
      readdir wiki_dir p |>
      List.map Version.parse |>
      List.sort (fun x y -> Version.compare y x) |> function
      | [] ->
        Printf.eprintf "no versions found for %s...\n%!" p;
        []
      | Version.Dev :: latest :: _
      | latest :: _ ->
        [p, latest]
    ) |>
    List.flatten;
  ids :=
    !projects |> List.map (fun (p, _) ->
      let full =
        let project_full = Filename.concat wiki_dir p in
        Filename.concat project_full "config.js"
      in
      try
        let f = Yojson.Safe.from_file full in
        let id =
          Yojson.Safe.Util.member "wiki_id" f |>
          Yojson.Safe.Util.to_int
        in
        (* FIXME *)
        let default_subproject =
          Yojson.Safe.Util.member "default_subproject" f |>
          Yojson.Safe.Util.to_string_option
        in
        [id, p]
      with _ ->
        Printf.eprintf "couldn't get %s's wiki_id...\n%!" p;
        []
    ) |>
    List.flatten

let latest_of project =
  List.assoc project !projects

let of_id id =
  List.assoc id !ids
