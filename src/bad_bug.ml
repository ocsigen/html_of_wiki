let () =
  let fmt = Format.formatter_of_out_channel stdout in
  let page = Tyxml.Html.(p [pcdata "hello"]) in
  Tyxml.Html.pp_elt () fmt page;
  Format.pp_print_flush fmt ()
  (* haha, not a bug. *)
