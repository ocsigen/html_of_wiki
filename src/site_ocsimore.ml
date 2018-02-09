(* Ocsigen web site
 * Copyright (C) 2009 Vincent Balat
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

open How_lib
open Lwt.Infix
open Tyxml

(*****************************************************************************)
(** Extension script *)

let do_script bi args c =
  `Flow5
    (try
      let src = List.assoc "src" args in
      Lwt.return Html.[
        script ~a:[ a_mime_type "text/javascript"; a_src src]
               (Html.cdata_script "")
      ]
    with Not_found ->
      let content =
        match c with
        | Some c -> c
        | None -> ""
      in
      Lwt.return Html.[
        script ~a:[a_mime_type "text/javascript"]
               (cdata_script content)
      ])


(*****************************************************************************)
(** Extension wip *)
(* Work in progress *)

let get_inline bi args =
  List.mem_assoc "inline" args

let do_wip bi args xml =
  `Flow5
    (let%lwt xml = match xml with
      | Some c -> (c :> Html_types.flow5 Html.elt list Lwt.t)
      | None -> Lwt.return [] in
     Lwt.return Html.[
       aside ~a:[a_class ["wip"]]
         (header [h5 [pcdata "Work in progress"]] :: xml)
      ])

let do_wip_inline bi args xml =
  `Phrasing_without_interactive
    (let%lwt xml = match xml with
       | Some c -> (c :> Html_types.phrasing Html.elt list Lwt.t)
       | None -> Lwt.return [] in
     let t =
       try List.assoc "title" args
       with Not_found -> "WIP: " in
     Lwt.return Html.[
       span ~a:[a_class ["wip"]]
            (strong [pcdata t] :: xml)
     ])


(*****************************************************************************)
(** Extension Concepts *)

let do_concepts bi args xml =
  `Flow5
    (let%lwt xml = match xml with
      | Some c -> (c :> Html_types.flow5 Html.elt list Lwt.t)
      | None -> Lwt.return [] in
     let attrs = Wiki_syntax.parse_common_attribs args in
     Lwt.return Html.[
       aside ~a:(a_class ["concepts"]::attrs)
             (header [h5 [pcdata "Concepts"]] :: xml)
     ])


(* Concept *)

let get_title bi args =
  try List.assoc "title" args
  with Not_found -> "Concept"

let do_concept bi args xml =
  `Flow5
    (let%lwt xml = match xml with
      | Some c -> (c :> Html_types.flow5 Html.elt list Lwt.t)
      | None -> Lwt.return [] in
     let t = get_title bi args in
     let attrs = Wiki_syntax.parse_common_attribs args in
     Lwt.return Html.[aside
       ~a:(a_class ["concept"]::attrs)
       (header [
          h5 [span ~a:[a_class ["concept_prefix"]]
                   [pcdata "Concept: "];
              pcdata t]
        ]
        :: xml)
      ])


(*****************************************************************************)
(* Extension paragraph *)

let do_paragraph bi args xml =
  `Flow5
    (let%lwt xml = match xml with
       | Some c -> (c :> _ Html.elt list Lwt.t)
       | None -> Lwt.return [] in
     let attrs = Wiki_syntax.parse_common_attribs args in
     Lwt.return Html.[div ~a:(a_class ["paragraph"]::attrs) xml])


(*****************************************************************************)
(** Extension Client/Server-Switch *)


let do_client_server_switch bi _ _ =
  `Flow5 (Lwt.return @@
    match bi.Wiki_widgets_interface.bi_page with
    | Document.Site _ -> []
    | Document.Deadlink _ -> assert false
    | Document.Project {project; version; page} ->
      match page with
      | Document.Api {subproject; file} ->
        let make_link subproject' =
          let page = Document.Api {subproject = subproject'; file} in
          let document = Document.Project {project; version; page} in
          bi.Wiki_widgets_interface.bi_add_link document;
          Html.[
            div ~a:[a_class ["client-server-switch-wrapper"]] [
              div ~a:[a_class ["client-server-switch"]] [
                span ~a:[a_class [subproject; "source"]]
                     [pcdata ("This is "^subproject^" API")];
                pcdata " API (go to ";
                a ~a:[a_class [subproject'; "target"];
                      a_href @@ Document.to_uri document]
                  [pcdata subproject'];
                pcdata ")"
              ]
            ]
          ]
        in
        (match subproject with
        | "server" -> make_link "client"
        | "client" -> make_link "server"
        | _ -> [])
      | _ -> []
  )


(*****************************************************************************)
(** Extension google search *)

let do_google_search _ _ _ =
  `Flow5
    (Lwt.return Html.[
      form ~a:[a_id "search"] [
        input ~a:[a_name "q"; a_id "q"; a_placeholder "search ..."] ();
        button [pcdata "Search"]
      ]
    ])


let init () =
  Wiki_syntax.register_simple_flow_extension
    ~name:"script" ~reduced:false do_script;
  Wiki_syntax.register_wiki_flow_extension ~reduced:false
    ~name:"wip" { Wiki_syntax.fpp = do_wip };
  Wiki_syntax.register_wiki_phrasing_extension ~reduced:false
    ~name:"wip-inline" { Wiki_syntax.ppp = do_wip_inline };
  Wiki_syntax.register_wiki_flow_extension
    ~name:"concepts" ~reduced:false { Wiki_syntax.fpp = do_concepts };
  Wiki_syntax.register_wiki_flow_extension
    ~name:"concept" ~reduced:false { Wiki_syntax.fpp = do_concept };
  Wiki_syntax.register_wiki_flow_extension
    ~reduced:false
    ~name:"paragraph"
    { Wiki_syntax.fpp = do_paragraph };
  Wiki_syntax.register_wiki_flow_extension
    ~reduced:false
    ~name:"client-server-switch"
    {Wiki_syntax.fpp = do_client_server_switch};
  Wiki_syntax.register_wiki_flow_extension
    ~name:"googlesearch" ~reduced:false { Wiki_syntax.fpp = do_google_search }
