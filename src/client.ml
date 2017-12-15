open Js_of_ocaml
open Tyxml
let () = Js.export_all (object%js


method outline {Bridge.elem; restrict; depth; ignore; nav; div} =
  let nav = (Dom_html.getElementById nav :> Dom.node Js.t) in
  let ignore = (fun (n: Dom.node Js.t) ->
    let tag = String.lowercase_ascii (Js.to_string n##.nodeName) in
    n == nav || List.mem tag ignore) in
  let elem, restrict =
    match elem with
    | `Id id ->
       ( (Dom_html.document##getElementById (Js.string id) :>
            Dom.node Js.t Js.opt),
         None )
    | `Container ->
      let fragment =
        if div then
          try
            let open Eliom_lib.Option in
            (HTML5outliner.find_previous_heading nav |> Js.Opt.to_option) >>=
            HTML5outliner.get_fragment
          with Not_found ->
            None
        else
          None
      in
      (HTML5outliner.find_container nav, fragment)
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
  Regexp.(global_replace (regexp "\xa0") s " ") |>
  Lexing.from_string |>
  Reason_toolchain.ML.implementation_with_comments |>
  Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter;
  Format.flush_str_formatter ()

(*
let has_class e c =
  e##.classList##contains (Js.string c) |>
  Js.to_bool
*)

let translate existing =
  match Js.Opt.to_option (existing##.textContent) with
  | None -> ()
  | Some ocaml ->
    let reason = ocaml |> Js.to_string |> to_reason |> Js.string in
    let code' =
      let c = Dom_html.(createCode document) in
      Dom.appendChild c (Dom_html.document##createTextNode reason);
      c##.className := Js.string "language-reason";
      c
    in
    insert_after ~existing code';
    Js.Unsafe.(fun_call (js_expr "Prism.highlightElement")
                        [| inject code' |]);
    (* remove translatable, so that we only do this once *)
    existing##.className := Js.string "language-ocaml"

let () =
  (* the search form isn't a real one... *)
  Dom_html.window##.onload := Dom_html.handler @@ fun _ ->
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
        let n = Js.string "body" in
        to_list (Dom_html.document##getElementsByTagName n) |>
        List.iter (fun body ->
          let empty = Js.string "" in
          let reason = Js.string "reason" in
          if body##.className = reason then (
            body##.className := empty
          )
          else (
            let t = Js.string "translatable" in
            to_list (Dom_html.document##getElementsByClassName t) |>
            List.iter translate;
            body##.className := reason
          )
        );
        Js.bool true);
    Js.bool true
