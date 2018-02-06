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
     let%lwt content = match c with
       | None -> Lwt.return []
       | Some c ->
         (Wiki_syntax.xml_of_wiki wp bi c :> Html_types.flow5 Html.elt list Lwt.t)
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
       let js = "outline(" ^ Buffer.contents buf ^ ")" in
       Html.(script @@ cdata_script js)
     in
   Lwt.return [nav; script])


let f_link bi args c =
  let wiki = Ocsimore_lib.list_assoc_default "wiki" args "" in
  let page = Ocsimore_lib.list_assoc_default "page" args "" in
  let fragment = Ocsimore_lib.list_assoc_opt "fragment" args in
  let content =
    match c with
    | Some c -> c
    | None -> Lwt.return [Html.pcdata page]
  in
  (* class and id attributes will be taken by Wiki_syntax.a_elem *)
  ( Wiki_syntax_types.Absolute
      (match fragment with
       | None -> Printf.sprintf "/%s/%s" wiki page
       | Some fragment -> Printf.sprintf "/%s/%s#%s" wiki page fragment),
    args,
    content )


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
  Wiki_syntax.register_link_phrasing_extension ~name:"link-inline" f_link
