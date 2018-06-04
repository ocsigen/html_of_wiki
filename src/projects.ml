exception No_such_project of string
exception No_such_id of int

type t = {
  name: string;
  versions: (Version.t * string list) list;
  latest: Version.t;
}


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

let init wiki_dir =
  projects :=
    readdir wiki_dir "" |>
    List.map (fun name ->
      let versions =
        readdir wiki_dir name |>
        List.map Version.parse |>
        List.sort (fun x y -> Version.compare y x)
      in
      match versions with
      | [] ->
        Printf.eprintf "no versions found for %s...\n%!" name;
        []
      | Version.Dev :: latest :: _
      | latest :: _ ->
        let p =
          let versions = versions |> List.map @@ fun v ->
            v,
            try
              readdir wiki_dir (name ++ (Version.to_string v) ++ "api")
            with Sys_error _ ->
              Printf.eprintf "no subprojects found for %s %s...\n%!"
                name
                (Version.to_string v);
              []
          in
          {name; versions; latest}
        in
        [name, p]
    ) |>
      List.flatten

let get project =
  try
    List.assoc project !projects
  with Not_found ->
    raise (No_such_project project)

let get_implicit_project bi =
  match bi.Wiki_widgets_interface.bi_page with
  | Document.Project {project; version; _} -> project, version
  | Document.Site _ -> failwith "no implicit project"
  | Document.Deadlink _ -> assert false
