open Tyxml
exception Error of string

let error (msg:string) =
  Lwt.return [ Html.span ~a:[Html.a_class ["doclink_error"]] [Html.txt msg] ]

let wrap_phrasing name f = fun bi args contents ->
  `Phrasing_without_interactive
    (let%lwt content =
       try%lwt
         f bi args contents
       with
       (* HACK comment references to module Document to avoid circular build *)
      | Error msg (* as exc *) ->
        (* bi.Wiki_widgets_interface.bi_add_link (Document.Deadlink exc); *)
        error (Format.sprintf "Error %s: %s" name msg)
      | exc ->
        (* bi.Wiki_widgets_interface.bi_add_link (Document.Deadlink exc); *)
        error (Format.sprintf "Error %s: exception %s" name
                  (Printexc.to_string exc) ) in
     Lwt.return [Html.span content])

let wrap_flow5 name f = fun bi args contents ->
  `Flow5
    (try%lwt
        f bi args contents
     with
       | Error msg -> error (Format.sprintf "Error %s: %s" name msg)
       | exc ->
         error (Format.sprintf "Error %s: exception %s" name
                  (Printexc.to_string exc) ) )
