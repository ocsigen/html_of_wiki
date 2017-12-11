let () =
  Links.init ();
  Only.init ();
  Menu.init ();
  Site_ocsimore.init ();
  Wiki_ext.init ();
  Code.init ()

let parse ~page ?title add_link content source = Wiki_syntax.(
  let parser = cast_wp wikicreole_parser in
  let bi_title = Eliom_lib.Option.default_to "" title in
  let bi = Wiki_widgets_interface.{
    bi_page = page;
    bi_sectioning = true;
    bi_add_link = add_link;
    bi_content = content;
    bi_title;
  } in
  xml_of_wiki parser bi source
)

let script_name = "/client.js"
let write_script fn =
  let ch = open_out fn in
  output_string ch [%blob "../client.js"];
  close_out ch

(* FIXME allow plugins to register scripts? *)
let scripts = [
  script_name; (* our client-side code *)
  "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-core.min.js";
  "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/components/prism-ocaml.min.js";
]

(* FIXME the same as above *)
let stylesheets = [
  "/style.css";
  "https://cdnjs.cloudflare.com/ajax/libs/prism/1.9.0/themes/prism.min.css";
]

let render ch ~title:t content =
  let fmt = Format.formatter_of_out_channel ch in
  let open Tyxml in
  let h =
    Html.(meta ~a:[a_charset "utf8"] () :: (
      List.map (fun s -> link ~rel:[`Stylesheet] ~href:s ()) stylesheets @
      List.map (fun s -> script ~a:[a_src s] (pcdata "")) scripts
    ))
  in
  Html.pp () fmt @@
    Html.(html
      (head (title (pcdata t)) h)
      (body content)
    );
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let rec flatten elt =
  let open Tyxml_xml in
  match content elt with
  | PCDATA _ -> [elt]
  | Entity _ -> [elt]
  | Node (name, a, children) ->
    List.map flatten children |>
    List.flatten
  | _ -> [] (* ignore the others *)

let extract_h1 blocks =
  let rec f = function
    | [] -> None
    | x :: t ->
      match Tyxml_xml.content x with
      | Tyxml_xml.Node ("h1", _, title) ->
        List.map flatten title |>
        List.flatten |>
        List.map (Format.asprintf "%a" (Tyxml_xml.pp ())) |>
        String.concat "" |> fun t ->
        Some t (* the first one, depth first *)
      | Tyxml_xml.Node (_, _, children) ->
        (match f children with
        | (Some title) as r -> r (* return the first one *)
        | None -> f t (* not found among children, try with the siblings *))
      | _ -> None (* not found at all *)
  in
  blocks |>
  List.map Tyxml_html.toelt |>
  f
