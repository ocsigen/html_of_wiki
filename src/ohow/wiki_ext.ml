(* Ocsimore
 * Copyright (C) 2005
 * Laboratoire PPS - Université Paris Diderot - CNRS
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
open Tyxml

let do_outline wp bi args c =
  `Flow5
    (let elem =
       try
         `Id (List.assoc "target" args)
       with Not_found -> `Container in
     let restrict =
       try Some (List.assoc "restrict" args)
       with Not_found -> None in
     let depth =
       try Some (int_of_string (List.assoc "depth" args))
       with _ -> None in
     let content = match c with
       | None -> []
       | Some c ->
         (Wiki_syntax.xml_of_wiki wp bi c :> Html_types.flow5 Html.elt list)
     in
     let ignore =
       Ocsimore_lib.get ~default:"nav aside" args "ignore" |>
       String.split_on_char ' ' |>
       List.map (fun x -> String.trim x |> String.lowercase_ascii)
     in
     let div =
       (elem = `Container && not bi.Wiki_widgets_interface.bi_sectioning)
       || List.mem_assoc "div" args
     in
     let id = "overview" in (* FIXME allow multiple blocks? *)
     let a =
       Html.a_id id ::
       Wiki_syntax.parse_common_attribs ~classes:["ocsimore_outline"] args
     in
     let nav = (if div then Html.div else Html.nav) ~a content in
     let script =
       let params = {Bridge.elem; restrict; depth; ignore; nav = id; div} in
       let buf = Buffer.create 256 in
       Bridge.outline_params_to_json buf params;
       let js = "window.addEventListener(\"load\", function(){ outline(" ^ Buffer.contents buf ^ "); }, false); " in
       Html.(script @@ cdata_script js)
     in
   [nav; script])

(* TODO: support for extended link syntax (wiki(toto):titi etc.) *)
let f_link _bi args c =
  let wiki = Ocsimore_lib.list_assoc_default "wiki" args "" in
  let page = Ocsimore_lib.list_assoc_default "page" args "" in
  let href = Ocsimore_lib.list_assoc_opt "href" args in
  let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
  let content =
    match c with
    | Some c -> c
    | None -> [Html.txt page]
  in
  (* class and id attributes will be taken by Wiki_syntax.a_elem *)
  ( Wiki_syntax_types.Absolute
      (match href, fragment with
       | Some href, _ -> href
       | _, None -> Printf.sprintf "/%s/%s" wiki page
       | _, Some fragment -> Printf.sprintf "/%s/%s#%s" wiki page fragment),
    args,
    content )

let do_drawer wp bi args c =
  let open Html in
  `Flow5
    (let attrs = Wiki_syntax.parse_common_attribs args in
     let content =
       match c with
       | Some c ->
         (Wiki_syntax.xml_of_wiki wp bi c
          :> Html_types.flow5 Html.elt list)
       | None -> []
     in
     let button = input ~a:[ a_id "how-drawer-toggle"
                           ; a_input_type `Checkbox ] ()
     in
     let label =
       label ~a:[ a_label_for "how-drawer-toggle" ; a_id "how-drawer-label" ]
         [ span ~a:[ a_class [ "how-drawer-icon" ] ] [] ]
     in
     let content = nav ~a:[ a_class [ "how-drawer-content" ] ] content in
     let elt = aside ~a:( a_class [ "how-drawer" ]
                          :: attrs) [ button ; label ; content ]
     in
     [ elt ])

let do_when_project _ _ args c =
  let open Utils.Operators in
  let fail e = failwith @@ "when_project: " ^ e in
  let opts = Extensions.get_opts ["when"; "unless"] args in
  let project, predicate = match opts with
    | [Some p; None] -> p, (=)
    | [None; Some p] -> p, (<>)
    | [None; None] -> fail "required arguments missing: \"when\" or \"unless\""
    | [Some _; Some _] -> fail "mutually incompatible arguments provided: \"when\", \"unless\""
    | _ -> fail "unexpected argument list provided"
  in
  (Global.options ()).project >>= (fun current ->
  if predicate project current
  then Some (`Flow5 ((c <$> Wiki_syntax.compile |? [])))
  else None)
                                  |? `Flow5 ([])

let do_when_local _ _ _ c =
  let open Utils.Operators in
  `Flow5 ((if (Global.options ()).local
           then c <$> Wiki_syntax.compile |? []
           else []))

let do_unless_local _ _ _ c =
  let open Utils.Operators in
  `Flow5 ((if not (Global.options ()).local
           then c <$> Wiki_syntax.compile |? []
           else []))

let init () =
  Wiki_syntax.register_raw_wiki_extension ~name:"outline"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    do_outline;
  Wiki_syntax.register_raw_wiki_extension ~name:"outline"
    ~wp:Wiki_syntax.wikicreole_parser_without_header_footer
    ~wp_rec:Wiki_syntax.wikicreole_parser_without_header_footer
    do_outline;
  Wiki_syntax.register_link_flow_extension ~name:"link"
    { Wiki_syntax.lfpp = f_link };
  Wiki_syntax.register_link_phrasing_extension ~name:"link-inline" f_link;
  Wiki_syntax.register_raw_wiki_extension ~name:"drawer"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    do_drawer;
  Wiki_syntax.register_raw_wiki_extension ~name:"when-project"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    do_when_project;
  Wiki_syntax.register_raw_wiki_extension ~name:"when-local"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    do_when_local;
  Wiki_syntax.register_raw_wiki_extension ~name:"unless-local"
    ~wp:Wiki_syntax.wikicreole_parser
    ~wp_rec:Wiki_syntax.wikicreole_parser
    do_unless_local
