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

let parse ~page add_link source = Wiki_syntax.(
  let parser = cast_wp wikicreole_parser in
  let bi = Wiki_widgets_interface.{
    bi_page = page;
    bi_sectioning = true;
    bi_add_link = add_link;
  } in
  xml_of_wiki parser bi source
)

let render ch ~header ~footer ~title content =
  let fmt = Format.formatter_of_out_channel ch in
  let title_re = Re.str "$TITLE" |> Re.compile in
  Format.print_string (Re.replace_string title_re ~by:title header);
  List.iter (Tyxml.Html.pp_elt () fmt) content;
  Format.print_string footer;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ()

let extract_h1 content =
  "HEY" (* FIXME maybe use a reference in Wiki_syntax... *)
