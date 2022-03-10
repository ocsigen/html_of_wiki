open Import

let read_project name dir =
  let versions = dir_files dir in
  let versions_t =
    List.map Version.parse versions |> List.sort Version.compare
  in
  let _latest =
    match
      List.filter
        (function
          | Version.Dev -> false
          | _ -> true)
        versions_t
    with
    | [] -> None
    | x :: _ -> Some x
  in
  let rec collect name version ?kind root dir =
    let files = dir_files dir in
    List.map
      (fun this_file ->
        let path = Filename.concat dir this_file in
        let fname = Filename.concat root path in
        if not (Sys.file_exists fname)
        then []
        else if Sys.is_directory fname
        then
          match (kind, this_file) with
          | None, "api" -> collect name version ~kind:`API root path
          | None, "manual" -> collect name version ~kind:`Manual root path
          | None, _ -> collect name version ~kind:`Page root path
          | Some `API, s -> collect name version ~kind:(`API_sub s) root path
          | Some kind, _ -> collect name version ~kind root path
        else
          let is_wiki = Filename.extension path = ".wiki" in
          match kind with
          | None | Some `Page -> [ Document.Static (path, `File) ]
          | Some `API ->
            if is_wiki
            then
              [ Document.Api
                  { subproject = None; file = Filename.chop_extension path }
              ]
            else [ Document.Static (path, `File) ]
          | Some (`API_sub subproject) ->
            if is_wiki
            then
              [ Document.Api
                  { subproject = Some subproject
                  ; file = Filename.chop_extension path
                  }
              ]
            else [ Document.Static (path, `File) ]
          | Some `Manual ->
            if is_wiki
            then [ Document.Manual (Filename.chop_extension path) ]
            else [ Document.Static (path, `File) ])
      files
    |> List.concat
  in
  List.map
    (fun version ->
      let dir = Filename.concat dir (Version.to_string version) in
      collect name version dir ".")
    versions_t
