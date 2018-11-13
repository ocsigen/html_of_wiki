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
open Utils.Operators

(*****************************************************************************)
(** Extension script *)

type script_kind = Src of string | Js of string

let make_script = function
  | Src src -> Html.(script ~a:[a_mime_type "text/javascript"; a_src src]
                       (cdata_script ""))
  | Js js -> Html.(script ~a:[a_mime_type "text/javascript"]
                     (cdata_script js))

let process_script args c = match List.assoc "src" args with
  | exception Not_found -> Js (c |? "")
  | src when Utils.is_none c -> Src src
  | _ -> failwith "script: both src and content are provided"

let do_script _ args c = `Flow5 ([process_script args c |> make_script] |> Lwt.return)

let head_scripts : script_kind list ref = ref []

let do_head_script _ args c =
  let script = process_script args c in
  head_scripts := script :: !head_scripts;
  `Flow5 (Lwt.return [])


(*****************************************************************************)
(** Extension css *)

type css_kind = Href of string | Css of string

let make_css = function
  | Href href -> Html.(link ~rel:[`Stylesheet] ~href ())
  | Css css -> Html.(style [pcdata css])

let process_css args c = match List.assoc "href" args with
  | exception Not_found -> Css (c |? "")
  | href when Utils.is_none c -> Href href
  | _ -> failwith "css: both href and content are provided"

let css_links : css_kind list ref = ref []

let do_head_css _ args c =
  let css = process_css args c in
  css_links := css :: !css_links;
  `Flow5 (Lwt.return [])


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

let do_client_server_switch _ args _ = match (Global.options ()).api with
  | None -> `Flow5 (Lwt.return [])
  | Some api ->
    let open Utils.Operators in
    let client = "client" in
    let server = "server" in
    let {Global.csw; root} = Global.options () in
    let file = Global.current_file () in
    let attrs = Wiki_syntax.parse_common_attribs args in
    let is_api = Paths.(is_inside_dir (root +/+ api) file) in
    let is_client = Paths.(is_inside_dir (root +/+ api +/+ client) file) in
    let is_server = Paths.(is_inside_dir (root +/+ api +/+ server) file) in
    let wiki = Filename.basename file in
    let make_switch = function
      | None -> []
      | Some other ->
        let html = Filename.chop_extension wiki ^ Global.suffix () in
        let href = Paths.(rewind (root +/+ api) file +/+ other +/+ html) in
        let checked = if other = server then [Html.a_checked ()] else [] in
        let onchange = "location = '" ^ href ^ "';" in
        Html.([label ~a:(a_class ["csw-switch"] :: attrs)
                 [input ~a:([a_input_type `Checkbox;
                             a_onchange onchange]
                            @ checked)
                    ();
                  span ~a:[a_class ["csw-slider"; "csw-slider-style-round"]] [];
                  span ~a:[a_class ["csw-slider-no"]]
                    [pcdata "Server version"];
                  span ~a:[a_class ["csw-slider-yes"]]
                    [pcdata "Client version"]]])
    in
    let make = function
      | [] -> []
      | wikis when is_api && List.exists (fun s -> s = wiki) wikis ->
        begin match is_client, is_server with
          | true, false -> make_switch @@ Some server
          | false, true -> make_switch @@ Some client
          | false, false -> make_switch None
          | _, _ -> assert false
        end
      | _ -> []
    in
    `Flow5 (Lwt.return (make csw))

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
  Wiki_syntax.register_simple_flow_extension
    ~name:"head-script" ~reduced:false do_head_script;
  Wiki_syntax.register_simple_flow_extension
    ~name:"head-css" ~reduced:false do_head_css;
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
