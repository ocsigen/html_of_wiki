(* ohow.ml - one html_of_wiki

   Converts a wikicreole file into an HTML file. See README.md for build
   instructions. *)

open Import
open Operators
open Markdown_builder

let build_page file content =
  let rec flatten elt =
    Tyxml_xml.(
      match content elt with
      | PCDATA _ -> [ elt ]
      | Entity _ -> [ elt ]
      | Node (_name, _a, children) -> List.map flatten children |> List.flatten
      | _ -> [])
    (* ignore the others *)
  in
  let extract_h1 blocks =
    let rec f = function
      | [] -> None
      | x :: t -> (
        match Tyxml_xml.content x with
        | Tyxml_xml.Node ("h1", _, title) ->
          List.map flatten title |> List.flatten
          |> List.map (Format.asprintf "%a" (Tyxml_xml.pp ()))
          |> String.concat ""
          |> fun t -> Some t
          (* the first one, depth first *)
        | Tyxml_xml.Node (_, _, children) -> (
          match f children with
          | Some _title as r -> r (* return the first one *)
          | None -> f t (* not found among children, try with the siblings *))
        | _ -> None)
      (* not found at all *)
    in
    f @@ List.map Tyxml_html.toelt blocks
  in
  let ti =
    match extract_h1 content with
    | Some s -> s
    | None -> ""
  in
  let a_cl =
    file
    ::
    (match (Global.options ()).project with
    | Some p -> [ p ]
    | None -> [])
  in
  Tyxml.Html.(
    html
      (head
         (title (txt ti))
         ([ meta ~a:[ a_charset "utf8" ] ()
          ; meta
              ~a:
                [ a_content "width=device-width, initial-scale=1"
                ; a_name "viewport"
                ]
              ()
          ]
         @ Site_ocsimore.(List.map make_css @@ List.rev !css_links)
         @ Site_ocsimore.(List.map make_script @@ List.rev !head_scripts)))
      (body ~a:[ a_class a_cl ] content))

let pprint ~indent oc html =
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp_elt ~indent () fmt html;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let infer_wiki_name = Filename.remove_extension

let infer_output_file file suffix =
  let base = infer_wiki_name file in
  base ^ suffix

let ohow ~indent file oc =
  ( ( file |> read_file |> fun wiki ->
      match (Global.options ()).template with
      | Some template ->
        read_file template |> Wiki_syntax.compile_with_content wiki
      | None -> Wiki_syntax.compile wiki )
  |> fun c ->
    if (Global.options ()).headless
    then List.iter (pprint ~indent oc) c
    else
      pprint ~indent oc
        (build_page (Filename.basename (infer_wiki_name file)) c) );
  close_out oc

let get_output_channel output_channel file suffix =
  match output_channel with
  | Some out -> out
  | None -> open_out @@ infer_output_file file suffix

let process_file opts output_channel file =
  Global.with_current_file file (fun () ->
      let oc =
        get_output_channel output_channel file
          (if opts.Global.out_language = "md" then ".md" else ".html")
      in
      match opts.Global.out_language with
      | "md" ->
        let content = read_file file in
        write_markdown oc content;
        close_out oc (* Ensure oc is closed after markdown conversion *)
      | "html" ->
        ohow ~indent:opts.Global.pretty file oc;
        close_out oc (* Ensure oc is closed after HTML conversion *)
      | _ ->
        ohow ~indent:opts.Global.pretty file oc;
        close_out oc)
(* Ensure oc is closed in the default case *)
(* Global.with_current_file file (fun () ->
 *     get_output_channel output_channel file
 *     |> ohow ~indent:opts.Global.pretty file) *)

let init_extensions () =
  Wiki_ext.init ();
  Links.init ();
  Code.init ();
  Menu.init ();
  Only.init ();
  Site_ocsimore.init ()

let main
    { Global.print
    ; pretty
    ; headless
    ; outfile
    ; suffix
    ; project
    ; root
    ; manual
    ; api
    ; default_subproject
    ; images
    ; assets
    ; template
    ; csw
    ; docversions
    ; local
    ; out_language
    ; files
    } =
  if not (List.for_all Sys.file_exists files)
  then failwith "Some input files doesn't exist...";
  init_extensions ();
  let root = Paths.realpath root in
  let relative_to_root p =
    try Paths.path_rm_prefix root @@ Paths.realpath p with Failure _ -> p
  in
  let manual = manual <$> relative_to_root in
  let api = api <$> relative_to_root in
  let images = images <$> relative_to_root in
  let assets = assets <$> relative_to_root in
  let opts =
    { Global.print
    ; pretty
    ; headless
    ; outfile
    ; suffix
    ; project
    ; root
    ; manual
    ; api
    ; default_subproject
    ; images
    ; assets
    ; template
    ; csw
    ; docversions
    ; local
    ; out_language
    ; files
    }
  in
  (match Sys.getenv_opt "HOW_IN_PROJECT" with
  | Some _ -> Global.root_to_site := Paths.(up +/+ up)
  | None -> ());
  Global.with_options opts (fun () ->
      (match (outfile, print) with
      | Some file, _ -> Some (open_out file)
      | None, true -> Some stdout
      | _ -> None)
      |> fun output_channel ->
      List.iter (fun file -> process_file opts output_channel file) files)

let () = Cli.run main
