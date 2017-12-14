open Tyxml

let attrs args =
  let open Ocsimore_lib in
  let lang = get_opt args "language" in
  match lang with
  | None ->
    [], []
  | Some lang ->
    let pre_classes, code_classes =
      if get_opt args "translated" = None && lang = "ocaml" then
        [], ["translatable"]
      else
        ["manually-translated"], []
    in
    [Html.a_class pre_classes],
    [Html.a_class @@ ("language-" ^ lang) :: code_classes]

let code bi args contents =
  `Flow5 (
    let contents = Eliom_lib.Option.default_to "" contents |> String.trim in
    let p_a, c_a = attrs args in
    (Lwt.return [Html.(pre ~a:p_a [code ~a:c_a [pcdata contents]])])
  )

let code_inline bi args contents =
  `Phrasing_without_interactive (
    let contents = Eliom_lib.Option.default_to "" contents |> String.trim in
    let _, c_a = attrs args in
    (Lwt.return [Html.(code ~a:c_a [pcdata contents])])
  )

let reason _ _ _ =
  `Flow5 Html.(
    Lwt.return [button ~a:[a_id "reason"] [pcdata "Reason"]]
  )


let init () =
  Wiki_syntax.register_raw_wiki_extension
    ~name:"reason-switch"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> reason);
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
