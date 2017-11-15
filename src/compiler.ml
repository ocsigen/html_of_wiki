let () = Links.init (); Only.init ()

let read_file ?(buffer_size=4096) filename =
  let ch = open_in filename in
  let buf = Buffer.create buffer_size in
  begin try
    while true do
      Buffer.add_string buf (input_line ch);
      Buffer.add_char buf '\n';
    done
  with End_of_file ->
    ()
  end;
  close_in ch;
  Buffer.to_bytes buf |> Bytes.to_string

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

let render ch ~title content =
  let fmt = Format.formatter_of_out_channel ch in
  let open Tyxml in
  Html.pp () fmt @@
    Html.html
      (Html.head (Html.title (Html.pcdata title)) [
        Html.meta ~a:[Html.a_charset "utf8"] ()
      ])
      (Html.body content);
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
