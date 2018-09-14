(* ohow.ml - one html_of_wiki

   Converts a wikicreole file into an HTML file.
   See README.md for build instructions.
*)

let compile text = Wiki_syntax.(
    let par = cast_wp wikicreole_parser in
    let bi = Wiki_widgets_interface.{
        bi_page = Site "";
        bi_sectioning = false;
        bi_add_link = (fun _ -> ());
        bi_content = Lwt.return [];
        bi_title = "";
      }
    in
    Lwt_main.run @@ xml_of_wiki par bi text)

let build_page content =
  let rec flatten elt = Tyxml_xml.(
      match content elt with
      | PCDATA _ -> [elt]
      | Entity _ -> [elt]
      | Node (name, a, children) -> List.map flatten children |> List.flatten
      | _ -> []) (* ignore the others *)
  in
  let extract_h1 blocks =
    let rec f = function
      | [] -> None
      | x :: t ->
        match Tyxml_xml.content x with
        | Tyxml_xml.Node ("h1", _, title) ->
          List.map flatten title
          |> List.flatten
          |> List.map (Format.asprintf "%a" (Tyxml_xml.pp ()))
          |> String.concat ""
          |> fun t -> Some t (* the first one, depth first *)
        | Tyxml_xml.Node (_, _, children) ->
          (match f children with
           | (Some title) as r -> r (* return the first one *)
           | None -> f t (* not found among children, try with the siblings *))
        | _ -> None (* not found at all *)
    in
    f @@ List.map Tyxml_html.toelt blocks
  in
  let ti = match extract_h1 content with
    | Some s -> s
    | None -> ""
  in
  Tyxml.Html.(html
                (head (title (pcdata ti)) [])
                (body content))

let readfile file =
  let ic = open_in file in
  let rec readall () =
    try
      let line = input_line ic in
      line :: readall ()
    with End_of_file -> []
  in
  let lines = readall() in
  close_in ic;
  String.concat "\n" lines

let pprint oc html =
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp_elt () fmt html;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let infer_wiki_name = Filename.remove_extension
let infer_output_file file = (infer_wiki_name file) ^ ".html"

let ohow file oc =
  file
  |> readfile
  |> compile
  |> build_page
  |> pprint oc;
  close_out oc

let get_output_channel output_channel file = match output_channel with
  | Some out -> out
  | None -> open_out @@ infer_output_file file

let process_file output_channel file =
  get_output_channel output_channel file |> ohow file

let main print outfile files =
  if List.for_all Sys.file_exists files
  then
    ((match (outfile, print) with
        | (Some file, _) -> Some (open_out file)
        | (None, true) -> Some stdout
        | _ -> None)
     |> process_file
     |> List.map) @@ files
  else
    (* NOTE should not happen because Cmdliner checks that *)
    failwith "Some input files doesn't exist..."

let () = Cmdliner.(
    let file_cmd =
      let doc = "The wikicreole file to convert to HTML." in
      Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc)
    in
    let print_cmd =
      let doc = "Print the HTML to stdout." in
      Arg.(value & flag & info ["p"; "print"] ~doc)
    in
    let outfile_cmd =
      let doc = "Writes the HTML into the given file. Always overwrites.
Overrides --print." in
      Arg.(value & opt (some string) None & info ["o"; "output"]
             ~docv:"FILE" ~doc)
    in
    let info_cmd =
      let doc = "Converts a wikicreole file into an HTML file." in
      let man = [] in
      Term.info "ohow" ~version:"v0.0.0" ~doc ~exits:Term.default_exits ~man
    in
    let ohow_cmd = Term.(const main $ print_cmd $ outfile_cmd $ file_cmd) in
    Term.(exit @@ eval (ohow_cmd, info_cmd)))
