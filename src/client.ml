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

let () =
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
    Js.bool true
