open Tyxml

let attrs args =
  let open Ocsimore_lib in
  let lang = get_opt args "language" in
  let translated = get_opt args "translated" <> None in
  match lang with
  | None -> []
  | Some lang ->
    Html.a_class ["language-" ^ lang] ::
    if translated then [Html.a_user_data "translated" ""] else []

let code bi args contents =
  `Flow5 (
    let contents = Eliom_lib.Option.default_to "" contents |> String.trim in
    (Lwt.return [Html.(pre ~a:(attrs args) [code [pcdata contents]])])
  )

let code_inline bi args contents =
  `Phrasing_without_interactive (
    let contents = Eliom_lib.Option.default_to "" contents |> String.trim in
    (Lwt.return [Html.(code ~a:(attrs args) [pcdata contents])])
  )


let init () =
  Wiki_syntax.register_raw_wiki_extension
    ~name:"code"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> code);
  Wiki_syntax.register_raw_wiki_extension
    ~name:"code-inline"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> code_inline);
