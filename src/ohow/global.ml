open Utils.Operators

let ref_current_file : string option ref = ref None

let with_current_file file k =
  ignore
    ( !ref_current_file
    >>= fun f ->
    let format =
      "Links.with_current_file \"%s\": file \"%s\" is currently being processed. \
       Refusing to override that value."
      ^^ ""
    in
    failwith @@ Printf.sprintf format file f );
  ref_current_file := Some file;
  let r = k () in
  ref_current_file := None;
  r

let using_current_file k =
  match !ref_current_file with
  | Some file -> k file
  | None -> failwith "Links.using_current_file: current_file is not set."

let current_file () = using_current_file Utils.id

type menu_file =
  | Manual of string
  | Api of string

let ref_menu_file : menu_file option ref = ref None

let with_menu_file mf k =
  ignore
    ( !ref_menu_file
    >>= (function Manual s | Api s -> Some s)
    >>= fun s -> failwith @@ "menu_file " ^ s ^ "already set. Abort." );
  ref_menu_file := Some mf;
  let r = k () in
  ref_menu_file := None;
  r

let using_menu_file k = !ref_menu_file <$> k

let menu_file () = !ref_menu_file

let manual_menu_file () = !ref_menu_file >>= function Manual s -> Some s | _ -> None

let api_menu_file () = !ref_menu_file >>= function Api s -> Some s | _ -> None

type cli_options =
  { files : string list
  ; print : bool
  ; headless : bool
  ; local : bool
  ; outfile : string option
  ; suffix : string
  ; project : string option
  ; root : string
  ; manual : string option
  ; api : string option
  ; default_subproject : string option
  ; images : string option
  ; assets : string option
  ; template : string option
  ; csw : string list
  ; docversions : string list }

let ref_options : cli_options option ref = ref None

let with_options opts k =
  let old = !ref_options in
  ref_options := Some opts;
  let r = k () in
  ref_options := old;
  r

let using_options k =
  match !ref_options with
  | Some options -> k options
  | None -> failwith "Global.options isn't properly intialized."

let options () = using_options Utils.id

let suffix () = (options ()).suffix

let the_manual () =
  match (options ()).manual with Some s -> s | None -> failwith "no manual given"

let the_api () =
  match (options ()).api with Some s -> s | None -> failwith "no api given"

let the_images () =
  match (options ()).images with Some s -> s | None -> failwith "no images given"

let the_assets () =
  match (options ()).assets with Some s -> s | None -> failwith "no assets given"

(* Preserve absolute path *)
let root () = (options ()).root

let version_dir = root

let project_dir () = version_dir () |> Filename.dirname

let all_projects_dir () = project_dir () |> Filename.dirname

let root_to_site = ref ""
