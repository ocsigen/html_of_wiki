let (++) = Filename.concat

let is_directory path name =
  name.[0] <> '.' && name.[0] <> '_' && name.[0] <> '#' &&
  Sys.is_directory @@ path ++ name

let readdir wiki_dir path =
  let full = wiki_dir ++ path in
  Sys.readdir full |>
  Array.to_list |>
  List.filter (fun name -> is_directory full name)

let projects = ref []
let ids = ref []

let init wiki_dir =
  projects :=
    readdir wiki_dir "" |>
    List.map (fun p ->
      let default_subproject =
        try
          let f =
            Yojson.Safe.from_file @@ wiki_dir ++ p ++ "config.js"
          in
          (try
            let id =
              Yojson.Safe.Util.member "wiki_id" f |>
              Yojson.Safe.Util.to_int
            in
            ids := (id, p) :: !ids
          with _ ->
            ());
          try
            Yojson.Safe.Util.member "default_subproject" f |>
            Yojson.Safe.Util.to_string
          with _ ->
            (* no default subproject *)
            ""
        with _ ->
          (* couldn't read config.js *)
          Printf.eprintf "couldn't read %s's config.js...\n%!" p;
          (* no default subproject *)
          ""
      in
      readdir wiki_dir p |>
      List.map Version.parse |>
      List.sort (fun x y -> Version.compare y x) |> function
      | [] ->
        Printf.eprintf "no versions found for %s...\n%!" p;
        []
      | Version.Dev :: latest :: _
      | latest :: _ ->
        [p, (default_subproject, latest)]
    ) |>
    List.flatten

let latest_of project =
  List.assoc project !projects |> snd

let of_id id =
  List.assoc id !ids

let default_subproject_of project =
  List.assoc project !projects |> fst
