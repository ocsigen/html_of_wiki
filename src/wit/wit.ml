let replace_content_tag tmpl content =
  let regexp = Re.Str.regexp "<<content>>" in
  try
    ignore @@ Re.Str.search_forward regexp tmpl 0;
    Some (Re.Str.substitute_first regexp (fun _ -> content) tmpl)
  with Not_found ->
    None

let read_channel_lines ic =
  let rec readall lines =
    try
      let line = input_line ic in
      readall (line :: lines)
    with End_of_file -> List.rev lines
  in
  readall []

let read_file_lines file =
  let ic = open_in file in
  let lines = read_channel_lines ic in
  close_in ic;
  lines

let read_in_channel ic = read_channel_lines ic |> String.concat "\n"
let read_file file = read_file_lines file |> String.concat "\n"


let main template =
  let tmpl = read_file template in
  let wiki = read_in_channel stdin in
  match replace_content_tag tmpl wiki with
  | Some replacement -> print_string replacement
  | None -> Printf.fprintf stderr "no <<content>> tag found in template\n"; exit 1


let tmpl_cmd =
  let doc = "The template to put the wiki into." in
  Cmdliner.Arg.(required & pos ~rev:true 0 (some file) None & info []
                  ~docv:"TMPL" ~doc)

let info_cmd = Cmdliner.(
    let doc = "Inlines a wikicreole file into another one with a <<content>> tag." in
    let man = [
      `S Manpage.s_description;
      `P "$(tname) reads wikicreole content from stdin and inserts it inside the \
          given template file $(b,TMPL) in place of the first <<content>> tag found and \
          outputs the result on stdout.";
      `P "The $(b,TMPL) file is never modified.";

      `S Manpage.s_bugs;
      `P "Escaping <<content>> tags has no effect, the tag is still recognized."
    ]
    in
    Term.info "wit" ~version:"v0.0.0" ~doc ~exits:Term.default_exits ~man)

let () =
  let wit_cmd = Cmdliner.Term.(const main $ tmpl_cmd) in
  Cmdliner.Term.(exit @@ eval (wit_cmd, info_cmd))
