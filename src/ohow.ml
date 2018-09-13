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
    } in
    Lwt_main.run @@ xml_of_wiki par bi text);;

let build_page content =
  let rec flatten elt =
    let open Tyxml_xml in
    match content elt with
    | PCDATA _ -> [elt]
    | Entity _ -> [elt]
    | Node (name, a, children) ->
      List.map flatten children |>
      List.flatten
    | _ -> [] (* ignore the others *)
  in
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
  in
  let ti = match extract_h1 content with
    | Some s -> s
    | None -> ""
  in
  Tyxml.Html.(html
                (head (title (pcdata ti)) [])
                (body content));;

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
  String.concat "\n" lines;;

let pprint oc html =
  let fmt = Format.formatter_of_out_channel oc in
  Tyxml.Html.pp_elt () fmt html;
  Format.pp_force_newline fmt ();
  Format.pp_print_flush fmt ();;

let infer_wiki_name = Filename.remove_extension;;
let inout_paths file = (file, (infer_wiki_name file) ^ ".html")

let ohow file =
  let (file_in, file_out) = inout_paths file in
  let oc = open_out file_out in
  (file_in
   |> readfile
   |> compile
   |> build_page
   |> pprint oc);
  close_out oc;;

let main file =
  if Sys.file_exists file then
    ohow file
  else
    failwith @@ "no such file: " ^ file;;

let open Cmdliner in
let file_t =
  let doc = "The wikicreole file to convert to HTML." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"FILE" ~doc)
in
let info_ =
  let doc = "Converts a wikicreole file into an HTML file." in
  let man = [] in
  Term.info "ohow" ~version:"v0.0.0" ~doc ~exits:Term.default_exits ~man
in
let ohow_t = Term.(const main $ file_t) in
Term.(exit @@ eval (ohow_t, info_));;
