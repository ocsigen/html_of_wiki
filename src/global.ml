open Utils.Operators

let ref_current_file : string option ref = ref None

let with_current_file file k =
  ignore (!ref_current_file >>= (fun f ->
      let format = "Links.with_current_file \"%s\": file \"%s\" is currently
being processed. Refusing to override that value." ^^ "" in
      failwith @@ Printf.sprintf format file f));
  ref_current_file := Some file;
  let r = k () in
  ref_current_file := None;
  r

let using_current_file k = match !ref_current_file with
  | Some file -> k file
  | None -> failwith "Links.using_current_file: current_file is not set."

let current_file () = using_current_file Utils.id


type cli_options = {
  files: string list;
  print: bool;
  outfile: string option;
  root: string;
  manual: string;
  api: string;
  images: string;
  assets: string;
}
let ref_options : cli_options option ref = ref None

let with_options opts k =
  let old = !ref_options in
  ref_options := Some opts;
  let r = k () in
  ref_options := old;
  r

let using_options k = match !ref_options with
  | Some options -> k options
  | None -> failwith "Cli.options isn't properly intialized."

let options () = using_options Utils.id

let root () = (options ()).root
let manual () = (options ()).manual
let api () = (options ()).api
let images () = (options ()).images
let assets () = (options ()).assets

let version_dir = root
let project_dir () = Pxu.path_of_list [version_dir (); Pxu.up]
let all_projects_dir () = Pxu.path_of_list [project_dir (); Pxu.up]
