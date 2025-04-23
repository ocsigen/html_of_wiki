open Import
open Tyxml

let attrs args =
  let attrs = Wiki_syntax.parse_common_attribs args in
  let lang = List.Assoc.get_opt args "language" in
  match lang with
  | None -> ([], [])
  | Some lang ->
      let pre_classes, code_classes =
        if List.Assoc.get_opt args "translated" = None && lang = "ocaml" then
          ([], [ "translatable" ])
        else ([ "manually-translated" ], [])
      in
      ( attrs @ [ Html.a_class pre_classes ],
        [ Html.a_class @@ (("language-" ^ lang) :: code_classes) ] )

let code _bi args contents =
  `Flow5
    (let contents =
       match contents with None -> "" | Some x -> String.trim x
     in
     let p_a, c_a = attrs args in
     [ Html.(pre ~a:p_a [ code ~a:c_a [ txt contents ] ]) ])

let code_inline _bi args contents =
  `Phrasing_without_interactive
    (let contents =
       match contents with None -> "" | Some x -> String.trim x
     in
     let _, c_a = attrs args in
     [ Html.(code ~a:c_a [ txt contents ]) ])

let reason _ _ _ =
  `Flow5 Html.[ button ~a:[ a_id "reason" ] [ txt "Switch to " ] ]

let init () =
  Wiki_syntax.register_raw_wiki_extension ~name:"reason-switch"
    ~wp:Wiki_syntax.wikicreole_parser ~wp_rec:Wiki_syntax.wikicreole_parser
    (fun _ -> reason);
  Wiki_syntax.register_simple_flow_extension ~name:"code" code;
  Wiki_syntax.register_simple_phrasing_extension ~name:"code-inline" code_inline
