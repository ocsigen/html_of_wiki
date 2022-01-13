open Utils.Operators
open Tyxml

let a_link_of_uri ?fragment suffix uri contents =
  let uri = suffix <$> (fun s -> Paths.concat_uri_suffix s uri) |? uri in
  let uri = uri ^ (fragment <$> (fun f -> "#" ^ f) |? "") in
  Html.a ~a:[ Html.a_href uri ] [ Html.txt (contents |? uri) ]

let manual_link contents = function
  | [ project; chapter; fragment; Some version ] ->
    let file = Global.current_file () in
    let root, manual = Global.(root (), the_manual ()) in
    let uri =
      match (project, chapter) with
      | Some p, Some c ->
        Paths.(
          rewind root file
          (* inside this version dir *)
          +/+ !Global.root_to_site
          (* inside project dir *)
          +/+ p
          +/+ version +/+ manual +/+ c)
      | Some p, None ->
        Paths.(rewind root file +/+ !Global.root_to_site +/+ p +/+ "index.html")
      | None, Some c -> Paths.(rewind root file +/+ manual +/+ c)
      | None, None -> failwith "a_manual: no project nor chapter arg found"
    in
    let link =
      match fragment with
      | Some fragment -> a_link_of_uri ~fragment
      | None -> a_link_of_uri ?fragment:None
    in
    [ link (Some (Global.suffix ())) uri contents ]
  | _ -> assert false

let api_link prefix contents = function
  | project :: subproject :: text :: Some version :: (([] | [ _ ]) as kind_opt)
    ->
    let kind =
      match (prefix, kind_opt) with
      | None, [ Some "odoc" ] -> `Odoc
      | None, [ Some "ocamldoc" ] -> `Ocamldoc
      | None, [ Some _ ] -> `Ocamldoc
      | None, [ None ] -> assert false
      | None, [] -> assert false
      | Some _, [] -> (
        let target_project =
          match project with
          | None -> Filename.basename (Global.project_dir ())
          | Some p -> p
        in
        match target_project with
        | "js_of_ocaml" -> (
          match version with
          | "latest" | "dev" -> `Odoc
          | v ->
            let v = Version.parse v in
            if Version.compare v (Version.parse "3.5.0") < 0
            then `Ocamldoc
            else `Odoc)
        | _ -> `Ocamldoc)
      | Some _, [ _ ] -> assert false
      | _, _ :: _ :: _ -> assert false
    in
    let file = Global.current_file () in
    let root, api = Global.(root (), the_api ()) in
    let id = Api.parse_contents (contents <$> String.trim) in
    let dsp = (Global.options ()).default_subproject in
    let base =
      match (project, subproject, dsp, kind) with
      | Some p, Some s, _, _ ->
        Paths.(
          rewind root file +/+ !Global.root_to_site +/+ p +/+ version +/+ api
          +/+ s)
      | Some p, None, _, `Ocamldoc ->
        Paths.(
          rewind root file +/+ !Global.root_to_site +/+ p +/+ version +/+ api)
      | Some p, None, _, `Odoc ->
        Paths.(
          rewind root file +/+ !Global.root_to_site +/+ p +/+ version +/+ api
          +/+ p)
      | None, Some s, _, _ | None, None, Some s, _ ->
        Paths.(rewind root file +/+ api +/+ s)
      | None, None, None, _ -> Paths.rewind root file +/+ api
    in
    let path_of_id =
      match (kind, prefix) with
      | `Ocamldoc, ((None | Some _) as prefix) ->
        Api.Ocamldoc.path_of_id ?prefix id
      | `Odoc, None -> Api.Odoc.path_of_id id
      | `Odoc, Some _ -> assert false
    in
    let uri = Filename.concat base @@ path_of_id in
    let fragment =
      match kind with
      | `Ocamldoc -> Api.Ocamldoc.fragment_of_id id
      | `Odoc -> Api.Odoc.fragment_of_id id
    in
    let body = text |? Api.string_of_id ~spacer:"." id in
    [ a_link_of_uri ?fragment (Some (Global.suffix ())) uri (Some body) ]
  | _ -> assert false

let img_link _contents = function
  | [ Some src ] ->
    let file = Global.current_file () in
    let root, images = Global.(root (), the_images ()) in
    let uri = Paths.(rewind root file +/+ images +/+ src) in
    let alt = Filename.basename src in
    [ Html.img ~src:uri ~alt () ]
  | [ None ] -> failwith "a_img: no src argument error"
  | _ -> assert false

let file_link contents = function
  | [ Some src ] ->
    let file = Global.current_file () in
    let root, assets = Global.(root (), the_assets ()) in
    let uri = Paths.(rewind root file +/+ assets +/+ src) in
    [ a_link_of_uri None uri (Some (contents |? Filename.basename uri)) ]
  | [ None ] -> failwith "a_file: no src argument error"
  | _ -> assert false

let init () =
  Extensions.(
    register "a_manual"
      [ "project"; "chapter"; "fragment"; "version" ]
      ~defaults:[ None; None; None; Some "latest" ]
      manual_link;
    register "a_api"
      [ "project"; "subproject"; "text"; "version"; "kind" ]
      ~defaults:[ None; None; None; Some "latest"; None ]
      (api_link None);
    [ "type"; "code" ]
    |> List.iter (fun p ->
           register ("a_api_" ^ p)
             [ "project"; "subproject"; "text"; "version" ]
             ~defaults:[ None; None; None; Some "latest" ]
             (api_link (Some (p ^ "_"))));
    register "a_img" [ "src" ] img_link;
    register "a_file" [ "src" ] file_link)
