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
