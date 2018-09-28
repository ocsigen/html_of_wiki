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

let pprint oc html =
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp_elt () fmt html;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let infer_wiki_name = Filename.remove_extension
let infer_output_file file = (infer_wiki_name file) ^ ".html"

let ohow file oc =
  file
  |> Utils.readfile
  |> Utils.compile
  |> build_page
  |> pprint oc;
  close_out oc

let get_output_channel output_channel file = match output_channel with
  | Some out -> out
  | None -> open_out @@ infer_output_file file


let process_file {Cli.root; manual; api; images; assets} output_channel file =
  Global.with_current_file file (fun () ->
      get_output_channel output_channel file |> ohow file)

let check_errors : (string * bool lazy_t) list -> unit =
  List.iter (fun (err, b) -> if Lazy.force b then () else failwith err)

let init_extensions () =
  Wiki_ext.init ();
  Links.init ();
  Code.init ();
  Menu.init ();
  Only.init ();
  Site_ocsimore.init ()

let main {Cli.print; outfile; root; manual; api; images; assets; files} =
  check_errors [("Some input files doesn't exist...",
                 lazy (List.for_all Sys.file_exists files))];
  init_extensions ();
  let root = Utils.realpath root in
  let relative_to_root p = Utils.path_rm_prefix root @@ Utils.realpath p in
  let manual = relative_to_root manual in
  let api = relative_to_root api in
  let images = relative_to_root images in
  let assets = relative_to_root assets in
  Cli.with_options {print; outfile; root; manual; api; images; assets; files}
    (fun () ->
       ((match (outfile, print) with
           | (Some file, _) -> Some (open_out file)
           | (None, true) -> Some stdout
           | _ -> None)
        |> process_file @@ Cli.options ()
        |> List.iter) @@ files)

let () = Cli.run main
