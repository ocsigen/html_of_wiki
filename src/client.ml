open Js_of_ocaml
open Tyxml
let () = Js.export_all (object%js


method outline {Bridge.elem; restrict; depth; ignore; nav; div} =
  let nav = (Dom_html.getElementById nav :> Dom.node Js.t) in
  let ignore = (fun (n: Dom.node Js.t) ->
    let tag = String.lowercase_ascii (Js.to_string n##.nodeName) in
    n == nav || List.mem tag ignore)
  in
  let elem, restrict2 =
    match elem with
    | `Id id ->
       ( (Dom_html.document##getElementById (Js.string id) :>
            Dom.node Js.t Js.opt),
         None )
    | `Container ->
      let fragment =
        if div then
          try
            let open How_lib.Option in
            (HTML5outliner.find_previous_heading nav |> Js.Opt.to_option) >>=
            HTML5outliner.get_fragment
          with Not_found ->
            None
        else
          None
      in
      (HTML5outliner.find_container nav, fragment)
  in
  let restrict = match restrict with
    | None -> restrict2
    | _ -> restrict
  in
  match Js.Opt.to_option elem with
  | None -> ()
  | Some elem ->
    let outline =
      HTML5outliner.outline ~ignore (Dom.list_of_nodeList elem##.childNodes)
    in
    let outline =
      match restrict with
      | Some fragment -> HTML5outliner.find_fragment fragment outline
      | None ->
        match outline with
        | [ HTML5outliner.Section(_,_,outline) ] -> outline
        | _ -> outline
    in
    Dom.appendChild nav (HTML5outliner.build_ol ?depth outline)


end)

let to_list l =
  let rec f acc i =
    if i < l##.length then
      match Js.Opt.to_option (l##item i) with
      | None -> f acc (i + 1)
      | Some x -> f (x :: acc) (i + 1)
    else
      List.rev acc
  in
  f [] 0

let remove_children n =
  while Js.to_bool n##hasChildNodes do
    Js.Opt.iter n##.lastChild (fun c -> ignore (n##removeChild c))
  done

let insert_after ~existing nw =
  Js.Opt.iter existing##.parentNode @@ fun p ->
  let parent = Js.Unsafe.coerce p in
  parent##insertBefore nw existing##.nextSibling

let to_reason s =
  let clean = Regexp.(global_replace (regexp "\xa0") s " ") in
  (try
    Lexing.from_string clean |>
    Reason_toolchain.ML.interface_with_comments |>
    Reason_toolchain.RE.print_interface_with_comments Format.str_formatter
  with _ ->
    Lexing.from_string clean |>
    Reason_toolchain.ML.implementation_with_comments |>
    Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter);
  Format.flush_str_formatter ()

(*
let has_class e c =
  e##.classList##contains (Js.string c) |>
  Js.to_bool
*)

let create_code ~language code =
  let c = Dom_html.(createCode document) in
  Dom.appendChild c (Dom_html.document##createTextNode code);
  c##.className := Js.string @@ "language-" ^ language;
  c

let highlight_element (node: Dom_html.element Js.t) : unit =
  Js.Unsafe.(fun_call (js_expr "Prism.highlightElement")
                      [| inject node |])

let translate existing =
  match Js.Opt.to_option (existing##.textContent) with
  | None -> ()
  | Some ocaml ->
    try
      (* to_bytestring is required because there are 0xa0 bytes *)
      let reason = ocaml |> Js.to_bytestring |> to_reason |> Js.string in
      let code' = create_code ~language:"reason" reason in
      insert_after ~existing code';
      highlight_element code';
      (* remove translatable, so that we only do this once *)
      existing##.className := Js.string "language-ocaml"
    with e ->
      existing##.className := Js.string "language-ocaml error"

let convert pre =
  let code = Dom_html.(createCode document) in
  code##.className := Js.string "language-ocaml translatable";
  code##.innerHTML := pre##.innerHTML;
  remove_children pre;
  Dom.appendChild pre code;
  pre##.className := Js.string "language-ocaml"

let remove_error_message n =
  let p = Js.Unsafe.coerce n in
  if (Js.string p##.firstChild == Js.string "[object Text]" && p##.firstChild##.data == Js.string "(* Error while translating to Reason *) \n") then (
    Js.Opt.iter n##.firstChild (fun c -> ignore (n##removeChild c)) (*clean this*)
  )

let add_error_message n =
  let t = Dom_html.document##createTextNode (Js.string "(* Error while translating to Reason *) \n") in
  let parent = Js.Unsafe.coerce n in
  parent##insertBefore t n##.firstChild

let toggle_reason () =
  let n = Js.string "body" in
  to_list (Dom_html.document##getElementsByTagName n) |>
  List.iter (fun body ->
    let reason = Js.string "reason" in
    if body##.className = reason then (
      let t = Js.string "language-ocaml error" in
      to_list (Dom_html.document##getElementsByClassName t) |>
      List.iter remove_error_message;
      body##.className := Js.string ""
    )
    else (
      let t = Js.string "translatable" in
      to_list (Dom_html.document##getElementsByClassName t) |>
      List.iter translate;
      let t = Js.string "error" in
      to_list (Dom_html.document##getElementsByClassName t) |>
      List.iter add_error_message;
      body##.className := reason

    )
  )

let () =
  (* the search form isn't a real one... *)
  ignore @@ Dom.addEventListener
    Dom_html.window
    (Dom_html.Event.make "load")
    (Dom_html.handler @@ fun _ ->
     (match Dom_html.(getElementById_coerce "search" CoerceTo.form) with
      | None -> ()
      | Some form ->
        form##.onsubmit := Dom_html.handler @@ fun _ ->
          let engine = "https://google.com/search?q=" in
          let filter = " site:ocsigen.org" in
          let q =
            (match Dom_html.(getElementById_coerce "q" CoerceTo.input) with
             | None -> filter
             | Some q -> Js.to_string q##.value ^ filter) |>
            Js.string |>
            Js.encodeURIComponent |>
            Js.to_string
          in
          Dom_html.window##.location##.href := Js.string (engine ^ q);
          Js.bool false);
     (* language switch *)
     (match Dom_html.getElementById_opt "reason" with
      | None -> ()
      | Some btn ->
        btn##.onclick := Dom_html.handler @@ fun _ ->
          toggle_reason ();
          Js.bool true);
     (* API conversion *)
     let f = Js.string "odocwiki_code" in
     to_list (Dom_html.document##getElementsByClassName f) |>
     List.iter convert;
     (* done! *)
     Js._true
    )
    Js._false
