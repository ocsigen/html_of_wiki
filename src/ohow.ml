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
  let stylesheets = ["bootstrap_custom.css"; "style.css"; "syntax.css"] |> List.map (fun f -> "https://ocsigen.github.io/css/" ^ f) in
  let scripts = ["https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-core.min.js";
                 "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-ocaml.min.js";
                 "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-clike.min.js";
                 "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-reason.min.js";
                 "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-javascript.min.js";
                 "https://ocsigen.github.io/js/client.js";
                 "https://ocsigen.github.io/js/viewport_ie10_hack.js"]
  in
  Tyxml.Html.(html
                (head (title (pcdata ti))
                   ([meta ~a:[a_charset "utf8"] ()]
                    @ (List.map (fun href -> link ~rel:[`Stylesheet] ~href ()) stylesheets)
                    @ (List.map (fun src -> script ~a:[a_src src] (pcdata "")) scripts)))
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
  |> Wiki_syntax.compile
  |> build_page
  |> pprint oc;
  close_out oc

let get_output_channel output_channel file = match output_channel with
  | Some out -> out
  | None -> open_out @@ infer_output_file file


let process_file {Global.root; manual; api; images; assets} output_channel file =
  Global.with_current_file file (fun () ->
      get_output_channel output_channel file |> ohow file)


let init_extensions () =
  Wiki_ext.init ();
  Links.init ();
  Code.init ();
  Menu.init ();
  Only.init ();
  Site_ocsimore.init ()

let main {Global.print; outfile; suffix; root; manual; api; images; assets; files} =
  Utils.check_errors [("Some input files doesn't exist...",
                       lazy (List.for_all Sys.file_exists files))];
  init_extensions ();
  let root = Paths.realpath root in
  let relative_to_root p = Paths.path_rm_prefix root @@ Paths.realpath p in
  let manual = relative_to_root manual in
  let api = relative_to_root api in
  let images = relative_to_root images in
  let assets = relative_to_root assets in
  let opts = {Global.print; outfile; suffix; root; manual; api; images; assets; files} in
  Global.with_options opts
    (fun () ->
       ((match (outfile, print) with
           | (Some file, _) -> Some (open_out file)
           | (None, true) -> Some stdout
           | _ -> None)
        |> process_file opts
        |> List.iter) @@ files)

let () = Cli.run main
