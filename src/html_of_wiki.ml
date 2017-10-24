open Tyxml.Html
let () = Api.init ()

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

let parse ~wiki ~page source = Wiki_syntax.(
  let parser = cast_wp wikicreole_parser in
  let bi = Wiki_widgets_interface.{
    bi_wiki = wiki;
    bi_page = wiki, page;
    bi_sectioning = true;
    bi_content = Lwt.return []
  } in
  xml_of_wiki parser bi source
)

let render ~header ~footer ~title content =
  let fmt = Format.formatter_of_out_channel stdout in
  let title_re = Re.str "$TITLE" |> Re.compile in
  Format.print_string (Re.replace_string title_re ~by:title header);
  List.iter (Tyxml.Html.pp_elt () fmt) content;
  Format.print_string footer;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let parse_path path =
  "w", "page" (* FIXME *)

let extract_h1 content =
  "HEY" (* FIXME *)


let () = Lwt_main.run (
  let filename = Sys.argv.(1) in
  let wiki, page = parse_path filename in
  let header = read_file "header" in (* FIXME *)
  let footer = read_file "footer" in (* FIXME *)
  let inp = read_file filename in
  let%lwt content = parse ~wiki ~page inp in
  let title = extract_h1 content in
  render ~header ~footer ~title content;
  Lwt.return_unit
)
