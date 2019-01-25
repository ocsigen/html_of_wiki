(* ohow.ml - one html_of_wiki

   Converts a wikicreole file into an HTML file.
   See README.md for build instructions.
*)
open Utils.Operators

let build_page content =
  let rec flatten elt = Tyxml_xml.(
      match content elt with
      | PCDATA _ -> [elt]
      | Entity _ -> [elt]
      | Node (_name, _a, children) -> List.map flatten children |> List.flatten
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
           | (Some _title) as r -> r (* return the first one *)
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
                (head (title (txt ti))
                   ([ meta ~a:[a_charset "utf8"] ()
                    ; meta ~a:[ a_content "width=device-width, initial-scale=1"
                              ; a_name "viewport" ] () ]
                    @ (Site_ocsimore.(List.map make_css @@ List.rev !css_links))
                    @ (Site_ocsimore.(List.map make_script @@ List.rev !head_scripts))))
                (body content))

let pprint oc html =
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp_elt () fmt html;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let infer_wiki_name = Filename.remove_extension
let infer_output_file file = (infer_wiki_name file) ^ ".html"

let ohow file oc =
  file
  |> Utils.read_file
  |> (fun wiki -> match (Global.options ()).template with
      | Some template ->
        Utils.read_file template |> Wiki_syntax.compile_with_content wiki
      | None -> Wiki_syntax.compile wiki)
  |> (fun c ->
      if (Global.options ()).headless
      then List.iter (pprint oc) c
      else pprint oc (build_page c));
  close_out oc

let get_output_channel output_channel file = match output_channel with
  | Some out -> out
  | None -> open_out @@ infer_output_file file


let process_file _opts output_channel file =
  Global.with_current_file file (fun () ->
      get_output_channel output_channel file |> ohow file)


let init_extensions () =
  Wiki_ext.init ();
  Links.init ();
  Code.init ();
  Menu.init ();
  Only.init ();
  Site_ocsimore.init ()

let main {Global.print; headless; outfile; suffix; project; root;
          manual; api; default_subproject; images; assets;
          template; csw; docversions; local; files} =
  Utils.check_errors [("Some input files doesn't exist...",
                       lazy (List.for_all Sys.file_exists files))];
  init_extensions ();
  let root = Paths.realpath root in
  let relative_to_root p =
    try Paths.path_rm_prefix root @@ Paths.realpath p
    with Failure _ -> p
  in
  let manual = manual <$> relative_to_root in
  let api = api <$> relative_to_root in
  let images = images <$> relative_to_root in
  let assets = assets <$> relative_to_root in
  let opts = {Global.print; headless; outfile; suffix; project; root;
              manual; api; default_subproject; images; assets;
              template; csw; docversions; local; files} in
  begin match Sys.getenv_opt "HOW_IN_PROJECT" with
    | Some _ -> Global.root_to_site := Paths.(up +/+ up)
    | None -> ()
  end;
  Global.with_options opts
    (fun () ->
       ((match (outfile, print) with
           | (Some file, _) -> Some (open_out file)
           | (None, true) -> Some stdout
           | _ -> None)
        |> process_file opts
        |> List.iter) @@ files)

let () = Cli.run main
