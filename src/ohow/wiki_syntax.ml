(* FIXME indentation *)
(* Ocsimore
 * Copyright (C) 2008
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
(** Pretty print wiki to HTML5 using Eliom's TyXML

    @author Vincent Balat
    @author Boris Yakobowski *)

open Ocsimore_lib
open Wiki_types
open Wiki_syntax_types
open Wiki_widgets_interface
open Tyxml
open Common

let class_wikibox wb = Printf.sprintf "wikiboxcontent%s" (string_of_wikibox wb)

let string_of_extension name args content =
  "<<" ^ name ^ " "
  ^ String.concat " " (List.map (fun (n, v) -> n ^ "=\"" ^ v ^ "\"") args)
  ^ (match content with
    | None -> ""
    | Some content -> "|" ^ content)
  ^ ">>"

let opt_of_list = function
  | [] -> None
  | _ :: _ as l -> Some l

(***)

let rec filter_raw = function
  (* /!\ NOT TAIL REC /!\ *)
  | [] -> []
  | None :: xs -> filter_raw xs
  | Some x :: xs -> x :: filter_raw xs

let unopt ~def = function
  | None -> def
  | Some x -> x

let parse_common_attribs ?classes attribs =
  let at1 =
    try
      Some
        (Html.a_class
           (Re.split spaces (List.assoc "class" attribs) @ unopt ~def:[] classes))
    with Not_found -> Option.map ~f:Html.a_class classes
  and at2 =
    try Some (Html.a_id (List.assoc "id" attribs)) with Not_found -> None
  and at3 =
    try Some (Html.a_style (List.assoc "style" attribs))
    with Not_found -> None
  in
  let data_attribs =
    List.fold_left
      (fun l (n, v) ->
        try
          if String.sub n 0 5 = "data-"
          then Html.a_user_data (String.sub n 5 (String.length n - 5)) v :: l
          else l
        with Invalid_argument _ -> l)
      [] attribs
  in
  filter_raw [ at1; at2; at3 ] @ data_attribs

let parse_table_cell_attribs attribs =
  let atts = parse_common_attribs attribs
  and at5 =
    try Some (Html.a_colspan (int_of_string (List.assoc "colspan" attribs)))
    with Not_found | Failure _ -> None
  and at6 =
    try Some (Html.a_headers [ List.assoc "headers" attribs ])
    with Not_found -> None
  and at7 =
    try Some (Html.a_rowspan (int_of_string (List.assoc "rowspan" attribs)))
    with Not_found | Failure _ -> None
  in
  atts @ filter_raw [ at5; at6; at7 ]

let item_builder ((c : Html_types.phrasing Html.elt list list), l, attribs) =
  let a = opt_of_list (parse_common_attribs attribs) in
  let r = List.flatten c in
  let l = unopt ~def:[] l in
  Html.li ?a
    ((r
       : Html_types.phrasing Html.elt list
       :> Html_types.li_content_fun Html.elt list)
    @ (l :> Html_types.li_content_fun Html.elt list))

let item_builder =
  (item_builder (* opening types *)
    : Html_types.phrasing Html.elt list list * _ * _ -> _
    :> [< Html_types.phrasing ] Html.elt list list * _ * _ -> _)

let list_builder xs =
  match xs with
  | [] -> (Html.li [], [])
  | x :: xs ->
    let y = item_builder x in
    let ys = List.map item_builder xs in
    (y, ys)

let ddt_builder (istitle, (d : Html_types.phrasing Html.elt list list), attribs)
    =
  let a = opt_of_list (parse_common_attribs attribs) in
  if istitle
  then Html.dt ?a (List.flatten d :> Html_types.dt_content_fun Html.elt list)
  else
    Html.dd ?a
      (List.flatten d
        : Html_types.phrasing Html.elt list
        :> Html_types.dd_content_fun Html.elt list)

let ddt_builder =
  (ddt_builder (* opening types *)
    : _ * Html_types.phrasing Html.elt list list * _ -> _
    :> _ * [< Html_types.phrasing ] Html.elt list list * _ -> _)

let descr_builder l = List.map ddt_builder l

type href = Wiki_syntax_types.href

let uri_of_href = function
  | Absolute s -> s
  | Document { document; fragment } -> Document.to_uri ?fragment document

let normalize_link _ _ _ _ = None

let starts_with prefix s =
  let p = String.length prefix in
  String.length s >= p && String.sub s 0 p = prefix

let ends_with suffix s =
  let l = String.length suffix in
  String.length s >= l && String.sub s (String.length s - l) l = suffix

let rec deabbrev_address = function
  (* [[]], [[#anchor]] *)
  | "" -> "href:"
  | anchor when starts_with "#" anchor -> deabbrev_address "" ^ anchor
  (* [[/some/path]] *)
  | abs when starts_with "/" abs -> "site:" ^ abs
  (* Already valid links *)
  | ok when starts_with "href:" ok -> ok
  | ok when starts_with "site:" ok -> ok
  | ok when starts_with "wiki:" ok -> ok
  | ok when Re.Pcre.(pmatch ~rex:(regexp "^wiki\\(.*\\):") ok) -> ok
  (* href: [[sub/page]] [[https://internet.com]] *)
  | href -> "href:" ^ href

let wiki_kind prot page =
  let is_number = Re.Pcre.(pmatch ~rex:(regexp "^\\d+$")) in
  let extract_wiki_name quoted_wiki =
    match Re.Pcre.(exec ~rex:(regexp "^\"([a-zA-Z0-9_-]+)\"$") quoted_wiki) with
    | exception Not_found -> failwith @@ "invalid wiki name: " ^ quoted_wiki
    | groups -> Re.Pcre.get_substring groups 1
  in
  let rex = Re.Pcre.regexp "^wiki\\((.*)\\)$" in
  match Re.Pcre.exec ~rex prot with
  | exception Not_found -> failwith @@ "ill formed wiki prototype: " ^ prot
  | groups -> (
    match Re.Pcre.get_substring groups 1 with
    | id when is_number id -> failwith "ids not supported anymore"
    | wiki ->
      let wiki = extract_wiki_name wiki in
      let file = Global.current_file () in
      let root = Global.root () in
      Absolute
        Paths.(rewind root file +/+ !Global.root_to_site +/+ wiki +/+ page))

let this_wiki_kind _prot page =
  let file = Global.current_file () in
  let root = Global.root () in
  Absolute Paths.(rewind root file +/+ page)

let link_kind _bi addr =
  match deabbrev_address addr |> String.split_on_char ':' with
  | p :: rest -> (
    let page = String.concat ":" rest in
    (* the page may contain ':' *)
    match p with
    | "href" ->
      let menu_page =
        Global.using_menu_file (fun mf ->
            let open Utils.Operators in
            let { Global.root; manual; api; _ } = Global.options () in
            let file = Global.current_file () in
            let is_manual =
              manual
              <$> (fun m -> Paths.(is_inside_dir (root +/+ m) file))
              |? false
            in
            let is_api =
              api
              <$> (fun a -> Paths.(is_inside_dir (root +/+ a) file))
              |? false
            in
            let manual, api = (manual |? "", api |? "") in
            match mf with
            | Manual _ when is_manual -> page
            | Api _ when is_api -> Paths.(rewind root file +/+ api +/+ page)
            | Manual _ when is_api ->
              Paths.(rewind root file +/+ manual +/+ page)
            | _ (* api when is_manual *) ->
              Paths.(rewind root file +/+ api +/+ page))
      in
      Absolute
        (let open Utils.Operators in
        menu_page |? page)
    | "site" ->
      let file = Global.current_file () in
      let root = Global.root () in
      Absolute
        Paths.(
          rewind root file +/+ !Global.root_to_site +/+ Utils.trim '/' page)
    | p when starts_with "wiki(" p -> wiki_kind p page
    | p when starts_with "wiki" p -> this_wiki_kind p page
    | _ -> failwith @@ "unknown prototype: '" ^ p ^ "'")
  | _ -> failwith @@ "ill formed link: '" ^ addr ^ "'"

let href_of_link_kind bi addr fragment =
  match link_kind bi addr with
  | Absolute a as h ->
    let open Utils.Operators in
    fragment <$> (fun f -> Absolute Paths.(a +/+ ("#" ^ f))) |? h
  | _ -> assert false

(** **)

type preparser = Wiki_syntax_types.preparser
type 'a wikicreole_parser = 'a Wiki_syntax_types.wikicreole_parser

type ('a, 'b, 'c) ext_wikicreole_parser =
  ('a, 'b, 'c) Wiki_syntax_types.ExtParser.ext_wikicreole_parser

open Wiki_syntax_types.ExtParser

(* cast ('a, 'b, 'c) ext_wikicreole_parser to 'a wikicreole_parser *)
let cast_wp (type a b c) wp =
  let module P = (val wp : ExtParser
                    with type res = a
                     and type res_without_interactive = b
                     and type link_content = c)
  in
  (module P : Parser with type res = a)

(* cast ('a, 'b, 'c) ext_wikicreole_parser to 'b wikicreole_parser *)
let cast_niwp (type a b c) wp =
  let module P = (val wp : ExtParser
                    with type res = a
                     and type res_without_interactive = b
                     and type link_content = c)
  in
  (module struct
    type res = P.res_without_interactive

    let from_string = P.from_string_without_interactive
    let preparse_string = P.preparse_string
    let desugar_string = P.desugar_string
  end : Parser
    with type res = b)

let get_plugin_resolver (type a b c) wp =
  let module P = (val wp : ExtParser
                    with type res = a
                     and type res_without_interactive = b
                     and type link_content = c)
  in
  P.plugin_resolver

let preparse_string (type a b c) wp =
  let module P = (val wp : ExtParser
                    with type res = a
                     and type res_without_interactive = b
                     and type link_content = c)
  in
  P.preparse_string

let desugar_string (type a b c) wp =
  let module P = (val wp : ExtParser
                    with type res = a
                     and type res_without_interactive = b
                     and type link_content = c)
  in
  P.desugar_string

(********************************)
(* Default parser functions:    *)

let xml_of_wiki (type t) wp bi content =
  let module Parser = (val wp : Parser with type res = t) in
  let xml = Parser.from_string ~sectioning:false bi content in
  List.flatten xml

let preprocess_extension (type t) wp =
  let module Parser = (val wp : Parser with type res = t) in
  (module Parser : Wiki_syntax_types.Preprocessor)

(*******)

module type RawParser = sig
  type res
  type res_without_interactive
  type text
  type link_content
  type list_item

  include
    Wikicreole.RawBuilder
      with type param := Wiki_widgets_interface.box_info
       and type href := href
       and type phrasing = text Html.elt list
       and type phrasing_without_interactive = link_content Html.elt list
       and type flow = res Html.elt list
       and type flow_without_interactive = res_without_interactive Html.elt list
       and type uo_list = list_item Html.elt list

  val ignore_a_elem_phrasing :
       Wikicreole.attribs
    -> href
    -> link_content Html.elt list list
    -> link_content Html.elt list

  val ignore_a_elem_flow :
       Wikicreole.attribs
    -> href
    -> res_without_interactive Html.elt list list
    -> res_without_interactive Html.elt list

  val default_extension :
       name:string
    -> Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> string option
    -> link_content Html.elt list

  val default_ni_extension :
       name:string
    -> Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> string option
    -> link_content Html.elt list
end

type (+'flow
     , +'flow_without_interactive
     , +'phrasing_without_interactive)
     plugin_content =
  [ `Flow5_link of
    href * Wikicreole.attribs * 'flow_without_interactive Html.elt list
  | `Phrasing_link of
    href * Wikicreole.attribs * 'phrasing_without_interactive Html.elt list
  | `Flow5 of 'flow Html.elt list
  | `Phrasing_without_interactive of 'phrasing_without_interactive Html.elt list
  ]

type (+'flow_without_interactive
     , +'phrasing_without_interactive)
     ni_plugin_content =
  [ `Flow5 of 'flow_without_interactive Html.elt list
  | `Phrasing_without_interactive of 'phrasing_without_interactive Html.elt list
  ]

type (+'flow_without_interactive
     , +'phrasing_without_interactive)
     link_plugin_content =
  [ `Flow5_link of
    href * Wikicreole.attribs * 'flow_without_interactive Html.elt list
  | `Phrasing_link of
    href * Wikicreole.attribs * 'phrasing_without_interactive Html.elt list
  ]

module MakeParser (B : RawParser) :
  ExtParser
    with type res = B.res
     and type res_without_interactive = B.res_without_interactive
     and type link_content = B.link_content = struct
  type res = B.res
  type res_without_interactive = B.res_without_interactive
  type link_content = B.link_content

  type wikiparser =
    (res, res_without_interactive, link_content) ext_wikicreole_parser

  type interactive_plugin_content =
    (res, res_without_interactive, link_content) plugin_content

  type simple_plugin =
       Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> string option
    -> (res, res_without_interactive, link_content) plugin_content

  type simple_ni_plugin =
       Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> string option
    -> (res_without_interactive, link_content) ni_plugin_content

  type 'a wiki_plugin =
       Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> 'a Html.elt list option
    -> (res, res_without_interactive, link_content) plugin_content

  type 'a wiki_ni_plugin =
       Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> 'a Html.elt list option
    -> (res_without_interactive, link_content) ni_plugin_content

  type 'a link_plugin =
       Wiki_widgets_interface.box_info
    -> Wikicreole.attribs
    -> 'a Html.elt list option
    -> (res_without_interactive, link_content) link_plugin_content

  (* Module to encode existential type parameter of the recursive wikiparser.
     Could be replaced by a GADT with Ocaml 3.13. *)
  module type WikiPlugin = sig
    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser :
      ( rec_res
      , rec_res_without_interactive
      , rec_link_content )
      ExtParser.ext_wikicreole_parser

    val update_context :
         Wiki_widgets_interface.box_info
      -> Wikicreole.attribs
      -> Wiki_widgets_interface.box_info

    val plugin : rec_res wiki_plugin
    val ni_plugin : rec_res_without_interactive wiki_ni_plugin option
  end

  module type LinkPlugin = sig
    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser :
      ( rec_res
      , rec_res_without_interactive
      , rec_link_content )
      ExtParser.ext_wikicreole_parser

    val update_context :
         Wiki_widgets_interface.box_info
      -> Wikicreole.attribs
      -> Wiki_widgets_interface.box_info

    val plugin : rec_res_without_interactive link_plugin
  end

  module type RawWikiPlugin = sig
    type rec_res
    type rec_res_without_interactive
    type rec_link_content

    val wikiparser :
      ( rec_res
      , rec_res_without_interactive
      , rec_link_content )
      ExtParser.ext_wikicreole_parser

    val plugin : rec_res wikicreole_parser -> simple_plugin

    val ni_plugin :
      (rec_res_without_interactive wikicreole_parser -> simple_ni_plugin) option
  end

  type plugin =
    | SimplePlugin of simple_plugin * simple_ni_plugin option
    | WikiPlugin of (module WikiPlugin)
    | LinkPlugin of (module LinkPlugin)
    | RawWikiPlugin of (module RawWikiPlugin)

  let plugin_assoc : (string, plugin * preparser option) Hashtbl.t =
    Hashtbl.create 17

  let register_extension ~name ?preparser plugin =
    Hashtbl.add plugin_assoc name (plugin, preparser)

  let rec plugin_resolver =
    Wikicreole.Resolver
      (fun name ->
        try
          match Hashtbl.find plugin_assoc name with
          | SimplePlugin _, _ -> None
          | WikiPlugin p, _ ->
            let module Plugin = (val p : WikiPlugin) in
            Some (get_plugin_resolver Plugin.wikiparser)
          | LinkPlugin p, _ ->
            let module Plugin = (val p : LinkPlugin) in
            Some (get_plugin_resolver Plugin.wikiparser)
          | RawWikiPlugin p, _ ->
            let module Plugin = (val p : RawWikiPlugin) in
            Some (get_plugin_resolver Plugin.wikiparser)
        with Not_found -> Some plugin_resolver)

  module InteractiveBuilder = struct
    include B

    type nonrec href = href
    type param = Wiki_widgets_interface.box_info
    type plugin_content = interactive_plugin_content

    let plugin_resolver = plugin_resolver

    let plugin name =
      try
        match Hashtbl.find plugin_assoc name with
        | SimplePlugin (plugin, _), _ -> (None, plugin)
        | WikiPlugin p, _ ->
          let module Plugin = (val p : WikiPlugin) in
          ( Some (get_plugin_resolver Plugin.wikiparser)
          , fun bi attribs content ->
              let bi = Plugin.update_context bi attribs in
              let xml =
                Option.map
                  ~f:(xml_of_wiki (cast_wp Plugin.wikiparser) bi)
                  (Option.map ~f:String.trim content)
              in
              Plugin.plugin bi attribs xml )
        | LinkPlugin p, _ ->
          let module Plugin = (val p : LinkPlugin) in
          ( Some (get_plugin_resolver Plugin.wikiparser)
          , fun bi attribs content ->
              let bi = Plugin.update_context bi attribs in
              let xml =
                Option.map
                  ~f:(xml_of_wiki (cast_niwp Plugin.wikiparser) bi)
                  (Option.map ~f:String.trim content)
              in
              (Plugin.plugin bi attribs xml
                :> ( res
                   , res_without_interactive
                   , link_content )
                   Wiki_syntax_types.plugin_content) )
        | RawWikiPlugin p, _ ->
          let module Plugin = (val p : RawWikiPlugin) in
          ( Some (get_plugin_resolver Plugin.wikiparser)
          , fun bi attribs content ->
              Plugin.plugin (cast_wp Plugin.wikiparser) bi attribs content )
      with Not_found ->
        ( Some plugin_resolver
        , fun bi attribs content ->
            `Phrasing_without_interactive
              (B.default_extension ~name bi attribs content) )

    let plugin = (plugin :> _ -> _ * (_ -> _ -> _ -> plugin_content))
    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
    let href_action _ _ _ _ _ = ()
  end

  let interactive_builder =
    (module InteractiveBuilder : Wikicreole.Builder
      with type param = Wiki_widgets_interface.box_info
       and type flow = B.flow)

  let from_string ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb interactive_builder content

  module NonInteractiveBuilder = struct
    type flow = B.flow_without_interactive
    type flow_without_interactive = B.flow_without_interactive
    type phrasing_without_interactive = B.phrasing_without_interactive
    type phrasing = B.phrasing_without_interactive
    type uo_list = B.uo_list
    type nonrec href = href
    type param = Wiki_widgets_interface.box_info

    let error = B.error
    let list = B.list
    let hr_elem = B.hr_elem
    let pre_elem = B.pre_elem
    let make_href = B.make_href
    let string_of_href = B.string_of_href
    let emdash = B.emdash
    let endash = B.endash
    let nbsp = B.nbsp
    let img_elem = B.img_elem
    let br_elem = B.br_elem
    let chars = B.chars
    let strong_elem att c = B.strong_elem att (List.map B.phrasing c)
    let em_elem att c = B.em_elem att (List.map B.phrasing c)
    let tt_elem att c = B.tt_elem att (List.map B.phrasing c)
    let monospace_elem att c = B.monospace_elem att (List.map B.phrasing c)
    let underlined_elem att c = B.underlined_elem att (List.map B.phrasing c)
    let linethrough_elem att c = B.linethrough_elem att (List.map B.phrasing c)
    let subscripted_elem att c = B.subscripted_elem att (List.map B.phrasing c)

    let superscripted_elem att c =
      B.superscripted_elem att (List.map B.phrasing c)

    let p_elem att c = B.p_elem att (List.map B.phrasing c)
    let h1_elem att c = B.h1_elem att (List.map B.phrasing c)
    let h2_elem att c = B.h2_elem att (List.map B.phrasing c)
    let h3_elem att c = B.h3_elem att (List.map B.phrasing c)
    let h4_elem att c = B.h4_elem att (List.map B.phrasing c)
    let h5_elem att c = B.h5_elem att (List.map B.phrasing c)
    let h6_elem att c = B.h6_elem att (List.map B.phrasing c)
    let section_elem att c = B.section_elem att (List.map B.flow c)
    let map_item (a, b, c) = (List.map B.phrasing a, b, c)
    let ul_elem att l = B.ul_elem att (List.map map_item l)
    let ol_elem att l = B.ol_elem att (List.map map_item l)
    let map_def (a, b, c) = (a, List.map B.phrasing b, c)
    let dl_elem att l = B.dl_elem att (List.map map_def l)
    let map_td (a, b, c) = (a, b, List.map B.phrasing c)
    let map_tr (a, b) = (List.map map_td a, b)
    let table_elem att l = B.table_elem att (List.map map_tr l)
    let phrasing x = x
    let flow x = x
    let a_elem_phrasing = B.ignore_a_elem_phrasing
    let a_elem_flow = B.ignore_a_elem_flow

    type plugin_content =
      [ `Flow5_link of href * Wikicreole.attribs * flow_without_interactive
      | `Phrasing_link of
        href * Wikicreole.attribs * phrasing_without_interactive
      | `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive
      ]

    let default_ni_plugin ~name bi attribs content =
      `Phrasing_without_interactive
        (B.default_ni_extension ~name bi attribs content)

    let plugin_resolver = plugin_resolver

    let plugin name =
      try
        match Hashtbl.find plugin_assoc name with
        | SimplePlugin (_, Some ni_plugin), _ -> (None, ni_plugin)
        | SimplePlugin (_, None), _ -> (None, default_ni_plugin ~name)
        | WikiPlugin p, _ -> (
          let module Plugin = (val p : WikiPlugin) in
          ( Some (get_plugin_resolver Plugin.wikiparser)
          , fun bi attribs content ->
              let bi = Plugin.update_context bi attribs in
              let xml =
                Option.map
                  ~f:(xml_of_wiki (cast_niwp Plugin.wikiparser) bi)
                  (Option.map ~f:String.trim content)
              in
              match Plugin.ni_plugin with
              | Some f -> f bi attribs xml
              | None -> default_ni_plugin ~name bi attribs content ))
        | LinkPlugin p, _ ->
          let module Plugin = (val p : LinkPlugin) in
          (Some (get_plugin_resolver Plugin.wikiparser), default_ni_plugin ~name)
        | RawWikiPlugin p, _ -> (
          let module Plugin = (val p : RawWikiPlugin) in
          ( Some (get_plugin_resolver Plugin.wikiparser)
          , fun bi attribs content ->
              match Plugin.ni_plugin with
              | Some f -> f (cast_niwp Plugin.wikiparser) bi attribs content
              | None -> default_ni_plugin ~name bi attribs content ))
      with Not_found -> (Some plugin_resolver, default_ni_plugin ~name)

    let plugin = (plugin :> _ -> _ * (_ -> _ -> _ -> plugin_content))
    let plugin_action _ _ _ _ _ _ = ()
    let link_action _ _ _ _ _ = ()
    let href_action _ _ _ _ _ = ()
  end

  let non_interactive_builder =
    (module NonInteractiveBuilder : Wikicreole.Builder
      with type param = Wiki_widgets_interface.box_info
       and type flow = B.flow_without_interactive)

  let from_string_without_interactive ~sectioning wb content =
    Wikicreole.from_string ~sectioning wb non_interactive_builder content

  (** Used to build the Preparser and Desugarer. *)
  module UnitBuilder = struct
    type href = string
    type phrasing_without_interactive = unit
    type phrasing = unit
    type flow = unit
    type flow_without_interactive = unit
    type uo_list = unit

    let nothing _ _ = ()
    let nothing1 _ = ()
    let chars = nothing1
    let strong_elem = nothing
    let em_elem = nothing
    let a_elem_phrasing _ _ _ = ()
    let a_elem_flow _ _ _ = ()
    let br_elem = nothing1
    let img_elem _ _ _ = ()
    let tt_elem = nothing
    let monospace_elem = nothing
    let underlined_elem = nothing
    let linethrough_elem = nothing
    let subscripted_elem = nothing
    let superscripted_elem = nothing
    let nbsp = ()
    let endash = ()
    let emdash = ()
    let p_elem = nothing
    let pre_elem = nothing
    let h1_elem = nothing
    let h2_elem = nothing
    let h3_elem = nothing
    let h4_elem = nothing
    let h5_elem = nothing
    let h6_elem = nothing
    let section_elem = nothing
    let ul_elem = nothing
    let ol_elem = nothing
    let dl_elem = nothing
    let list = nothing1
    let flow = nothing1
    let hr_elem = nothing1
    let table_elem = nothing
    let phrasing = nothing1
    let error = nothing1

    let make_href _ a fragment =
      match fragment with
      | None -> a
      | Some f -> a ^ "#" ^ f

    let string_of_href x = x

    type plugin_content =
      [ `Flow5_link of href * Wikicreole.attribs * flow_without_interactive
      | `Phrasing_link of
        href * Wikicreole.attribs * phrasing_without_interactive
      | `Flow5 of flow_without_interactive
      | `Phrasing_without_interactive of phrasing_without_interactive
      ]

    let plugin_resolver = plugin_resolver

    let plugin name =
      let wiki_content =
        try
          match Hashtbl.find plugin_assoc name with
          | SimplePlugin _, _ -> None
          | RawWikiPlugin _, _ -> Some plugin_resolver
          | WikiPlugin p, _ ->
            let module WikiPlugin = (val p : WikiPlugin) in
            Some (get_plugin_resolver WikiPlugin.wikiparser)
          | LinkPlugin p, _ ->
            let module LinkPlugin = (val p : LinkPlugin) in
            Some (get_plugin_resolver LinkPlugin.wikiparser)
        with Not_found -> Some plugin_resolver
      in
      (wiki_content, fun _ _ _ -> `Phrasing_without_interactive ())
  end

  (* Type of the substitutions collected by [desugar_string] and
     [preparse_string]. *)
  type substitutions = (int * int * string option) list ref

  let link_action_ref = ref (fun _ _ _ _ -> None)
  let href_action_ref = ref (fun _ _ _ _ -> None)

  let preparser =
    let module Preparser = struct
      type param = substitutions * Wiki_types.wikibox

      include UnitBuilder

      let plugin_action name start end_ (subst, wb) attribs content =
        try
          let plugin, preparser = Hashtbl.find plugin_assoc name in
          let content' =
            let content' =
              match plugin with
              | SimplePlugin _ -> content
              | WikiPlugin p -> (
                let module Plugin = (val p : WikiPlugin) in
                match content with
                | None -> None
                | Some content ->
                  let content = preparse_string Plugin.wikiparser wb content in
                  Some content)
              | LinkPlugin p -> (
                let module Plugin = (val p : LinkPlugin) in
                match content with
                | None -> None
                | Some content ->
                  let content = preparse_string Plugin.wikiparser wb content in
                  Some content)
              | RawWikiPlugin p -> (
                let module Plugin = (val p : RawWikiPlugin) in
                match content with
                | None -> None
                | Some content ->
                  let content = preparse_string Plugin.wikiparser wb content in
                  Some content)
            in
            match preparser with
            | None -> (
              match (content, content') with
              | None, None -> None
              | Some content, Some content' when content' == content -> None
              | _, _ -> Some (string_of_extension name attribs content'))
            | Some preparser -> preparser wb attribs content'
          in
          subst := (start, end_, content') :: !subst
        with _ (* was Not_found *) -> ()

      let link_action addr fragment attribs (start, end_) (subst, params) =
        subst :=
          ( start
          , end_
          , try !link_action_ref addr fragment attribs params with _ -> None )
          :: !subst

      let href_action addr fragment attribs (start, end_) (subst, params) =
        subst :=
          ( start
          , end_
          , try !href_action_ref addr fragment attribs params with _ -> None )
          :: !subst
    end in
    (module Preparser : Wikicreole.Builder
      with type param = substitutions * Wiki_types.wikibox
       and type flow = unit)

  let normalize_href_ref = ref normalize_link

  let desugarer =
    let module Desugarer = struct
      type param = substitutions * Wiki_syntax_types.desugar_param

      include UnitBuilder

      let plugin_action :
          string -> int -> int -> (param, unit) Wikicreole.plugin =
       fun name start end_ (subst, wb) attribs content ->
        let desugar_attributes () =
          let attribs' =
            let f = function
              | "item", it ->
                let it' =
                  let link, text =
                    try String.sep '|' it with Not_found -> (it, it)
                  in
                  match !normalize_href_ref (0, 0) link None wb with
                  | Some link' -> link' ^ "|" ^ text
                  | None -> it
                in
                ("item", it')
              | x -> x
            in
            List.map f attribs
          in
          if attribs' <> attribs
          then Some (string_of_extension name attribs' content)
          else None
        in
        let desugar_content desugar_string_with_parser =
          match content with
          | None -> None
          | Some content ->
            let content' = desugar_string_with_parser wb content in
            if content' <> content
            then Some (string_of_extension name attribs (Some content'))
            else None
        in
        try
          let plugin, _ = Hashtbl.find plugin_assoc name in
          let content' =
            match plugin with
            | SimplePlugin _ -> desugar_attributes ()
            | WikiPlugin p ->
              desugar_content
                (let module Plugin = (val p : WikiPlugin) in
                fun a b -> (desugar_string Plugin.wikiparser) a b)
            | LinkPlugin p ->
              desugar_content
                (let module Plugin = (val p : LinkPlugin) in
                fun a b -> (desugar_string Plugin.wikiparser) a b)
            | RawWikiPlugin p ->
              desugar_content
                (let module Plugin = (val p : RawWikiPlugin) in
                fun a b -> (desugar_string Plugin.wikiparser) a b)
          in
          subst := (start, end_, content') :: !subst
        with _ (* was Not_found *) -> ()

      let link_action :
          string -> string option -> _ -> int * int -> param -> unit =
       fun _ _ _ _ _ -> ()

      let href_action :
          string -> string option -> _ -> int * int -> param -> unit =
       fun addr fragment _ ((start, end_) as pos) (subst, wikipage) ->
        subst :=
          ( start
          , end_
          , try !normalize_href_ref pos addr fragment wikipage with _ -> None )
          :: !subst
    end in
    (module Desugarer : Wikicreole.Builder
      with type param = substitutions * Wiki_syntax_types.desugar_param
       and type flow = unit)

  let apply_subst subst content =
    let buf = Buffer.create 1024 in
    let pos =
      List.fold_left
        (fun pos (start, end_, replacement) ->
          match replacement with
          | None -> pos
          | Some replacement ->
            Buffer.add_substring buf content pos (start - pos);
            Buffer.add_string buf replacement;
            end_)
        0 subst
    in
    if pos < String.length content
    then Buffer.add_substring buf content pos (String.length content - pos);
    Buffer.contents buf

  let with_actions ?href_action ?link_action f =
    (* No mutex required: the "lexer" do not cooperate and any access to the
       reference take place before the call to [apply_subst] *)
    let old_link_action = !link_action_ref in
    let old_href_action = !href_action_ref in
    (match link_action with
    | Some f -> link_action_ref := f
    | None -> ());
    (match href_action with
    | Some f -> href_action_ref := f
    | None -> ());
    let res = f () in
    link_action_ref := old_link_action;
    href_action_ref := old_href_action;
    res

  let desugar_string ?href_action ?link_action wb content =
    with_actions ?href_action ?link_action (fun () ->
        let subst = ref [] in
        ignore
          (Wikicreole.from_string (subst, wb) desugarer content : unit list);
        apply_subst (List.rev !subst) content)

  let preparse_string ?href_action ?link_action wb content =
    with_actions ?href_action ?link_action (fun () ->
        let subst = ref [] in
        ignore
          (Wikicreole.from_string (subst, wb) preparser content : unit list);
        apply_subst (List.rev !subst) content)
end

let menu_make_href = href_of_link_kind

(*******************************************)
(* Type information for predefined parser. *)

module FlowTypes = struct
  type res = Html_types.flow5
  type res_without_interactive = Html_types.flow5_without_interactive
  type text = Html_types.phrasing
  type link_content = Html_types.phrasing_without_interactive

  type list_item =
    [ `Ol
    | `Ul
    | `Em
    ]
end

module FlowWithoutHeaderFooterTypes = struct
  type res = Html_types.flow5_without_header_footer

  type res_without_interactive =
    Html_types.flow5_without_interactive_header_footer

  type text = Html_types.phrasing
  type link_content = Html_types.phrasing_without_interactive

  type list_item =
    [ `Ol
    | `Ul
    | `Em
    ]
end

module PhrasingTypes = struct
  type res = Html_types.phrasing
  type res_without_interactive = Html_types.phrasing_without_interactive
  type text = Html_types.phrasing
  type link_content = Html_types.phrasing_without_interactive
  type list_item = [ `Em ]
end

module MenuTypes = struct
  type res =
    [ `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    ]

  type res_without_interactive =
    [ `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    ]

  type text = Html_types.phrasing
  type link_content = Html_types.phrasing_without_interactive

  type list_item =
    [ `H1
    | `H2
    | `H3
    | `H4
    | `H5
    | `H6
    ]
end

module ButtonTypes = struct
  type res =
    [ Html_types.button_content
    | `PCDATA
    ]

  type res_without_interactive =
    [ Html_types.button_content
    | `PCDATA
    ]

  type text =
    [ Html_types.button_content
    | `PCDATA
    ]

  type link_content =
    [ Html_types.button_content
    | `PCDATA
    ]

  type list_item =
    [ Html_types.button_content
    | `PCDATA
    ]
end

(********************************)
(* Predefined builders.         *)

module FlowBuilder = struct
  let chars s = [ Html.txt s ]

  let strong_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.strong ?a r : [> `Strong ] Html.elt) ]

  let em_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.em ?a r : [> `Em ] Html.elt) ]

  let monospace_elem attribs content =
    let a = Html.a_class [ "monospace" ] :: parse_common_attribs attribs in
    let r = List.flatten content in
    [ (Html.span ~a r : [> `Span ] Html.elt) ]

  (* FIXME use <u>? *)
  let underlined_elem attribs content =
    let a = Html.a_class [ "underlined" ] :: parse_common_attribs attribs in
    let r = List.flatten content in
    [ (Html.span ~a r : [> `Span ] Html.elt) ]

  let linethrough_elem attribs content =
    let a = Html.a_class [ "linethrough" ] :: parse_common_attribs attribs in
    let r = List.flatten content in
    [ (Html.span ~a r : [> `Span ] Html.elt) ]

  let subscripted_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.sub ?a r : [> `Sub ] Html.elt) ]

  let superscripted_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.sup ?a r : [> `Sup ] Html.elt) ]

  let a_elem_phrasing attribs addr
      (c : Html_types.phrasing_without_interactive Html.elt list list) =
    let a =
      parse_common_attribs ~classes:[ "ocsimore_phrasing_link" ] attribs
    in
    let address, text =
      match addr with
      | Absolute "" -> (Some "", Some ".")
      | Absolute a when Utils.uri_absolute a -> (Some a, None)
      | Absolute a when ends_with "/" a -> (Some a, None)
      | Absolute a -> (Some (a ^ Global.suffix ()), None)
      | _ -> assert false
    in
    let c = List.flatten c in
    [ (Html.a
         ~a:
           ( (let open Utils.Operators in
             (* NOTE address is always Some x for now but one could add another
                case to the matching above in which the original address is to
                be used. *)
             address <$> (fun a -> Absolute a) |? addr)
           |> uri_of_href |> Html.a_href
           |> fun x -> x :: a )
         (let open Utils.Operators in
         text >>= (fun t -> Some [ Html.txt t ]) |? c)
        :> Html_types.phrasing Html.elt)
    ]

  let a_elem_flow attribs addr c =
    let a = parse_common_attribs ~classes:[ "ocsimore_flow_link" ] attribs in
    let c = List.flatten c in
    [ Html.a ~a:(Html.a_href (uri_of_href addr) :: a) c ]

  let make_href = href_of_link_kind
  let string_of_href = uri_of_href

  let br_elem attribs =
    let a = opt_of_list (parse_common_attribs attribs) in
    [ (Html.br ?a () : [> `Br ] Html.elt) ]

  let img_elem attribs href alt =
    let a = opt_of_list (parse_common_attribs attribs) in
    let src = uri_of_href href (* CCC https ? *) in
    [ (Html.img ~src ~alt ?a () : [> `Img ] Html.elt) ]

  let tt_elem attribs content =
    let a = Html.a_class [ "teletype" ] :: parse_common_attribs attribs in
    let r = List.flatten content in
    [ (Html.span ~a r : [> `Span ] Html.elt) ]

  let nbsp = [ (Html.txt " " : [> `PCDATA ] Html.elt) ]
  let endash = [ (Html.txt "–" : [> `PCDATA ] Html.elt) ]
  let emdash = [ (Html.txt "—" : [> `PCDATA ] Html.elt) ]

  let p_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.p ?a r : [> `P ] Html.elt) ]

  let pre_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    [ (Html.pre ?a [ Html.txt (String.concat "" content) ] : [> `Pre ] Html.elt)
    ]

  let add_backref attribs r =
    if !Ocsimore_config.wiki_headings_backref
    then
      try
        let id = List.assoc "id" attribs in
        let open Html in
        r
        @ [ txt " "
          ; a ~a:[ a_class [ "backref" ]; a_href ("#" ^ id) ] [ entity "#182" ]
          ]
      with Not_found -> r
    else r

  let h1_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h1 ?a (add_backref attribs r) : [> `H1 ] Html.elt) ]

  let h2_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h2 ?a (add_backref attribs r) : [> `H2 ] Html.elt) ]

  let h3_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h3 ?a (add_backref attribs r) : [> `H3 ] Html.elt) ]

  let h4_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h4 ?a (add_backref attribs r) : [> `H4 ] Html.elt) ]

  let h5_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h5 ?a (add_backref attribs r) : [> `H5 ] Html.elt) ]

  let h6_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.h6 ?a (add_backref attribs r) : [> `H6 ] Html.elt) ]

  let section_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.section ?a (r :> Html_types.section_content_fun Html.elt list)
        : [> `Section ] Html.elt)
    ]

  let ul_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    list_builder content |> fun (r, rs) ->
    [ (Html.ul ?a (r :: rs) : [> `Ul ] Html.elt) ]

  let ol_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    list_builder content |> fun (r, rs) ->
    [ (Html.ol ?a (r :: rs) : [> `Ol ] Html.elt) ]

  let dl_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    descr_builder content |> fun r -> [ (Html.dl ?a r : [> `Dl ] Html.elt) ]

  let hr_elem attribs =
    let a = opt_of_list (parse_common_attribs attribs) in
    [ (Html.hr ?a () : [> `Hr ] Html.elt) ]

  let tdh_builder (h, attribs, (c : Html_types.phrasing Html.elt list list)) =
    let a = opt_of_list (parse_table_cell_attribs attribs) in
    let r = List.flatten c in
    if h
    then
      Html.th ?a
        (r
          : Html_types.phrasing Html.elt list
          :> Html_types.th_content_fun Html.elt list)
    else
      Html.td ?a
        (r
          : Html_types.phrasing Html.elt list
          :> Html_types.td_content_fun Html.elt list)

  let tdh_builder =
    (tdh_builder (* opening types *)
      : _ * _ * Html_types.phrasing Html.elt list list -> _
      :> _ * _ * [< Html_types.phrasing ] Html.elt list list -> _)

  let tr_builder (row, attribs) =
    match row with
    | [] -> Html.tr [ Html.td [] ]
    | x :: xs ->
      let a = opt_of_list (parse_common_attribs attribs) in
      (*let a = opt_of_list (parse_table_row_attribs attribs) in*)
      let y = tdh_builder x in
      let ys = List.map tdh_builder xs in
      Html.tr ?a (y :: ys)

  let table_elem attribs l =
    let a = opt_of_list (parse_common_attribs attribs) in
    let caption =
      try Some (Html.caption [ Html.txt (List.assoc "summary" attribs) ])
      with Not_found -> None
    in
    match l with
    | [] -> [ Html.table ?a ?caption [ Html.tr [ Html.td [] ] ] ]
    | rows ->
      let rows = List.map tr_builder rows in
      [ (Html.table ?a ?caption rows : [> `Table ] Html.elt) ]

  let error (s : string) =
    [ (Html.strong [ Html.txt s ] : [> `Strong ] Html.elt) ]

  let span_elem attribs content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.span ?a r : [> `Span ] Html.elt) ]

  let ignore_a_elem_phrasing attribs _ content = span_elem attribs content

  let ignore_a_elem_flow attribs _ content =
    let a = opt_of_list (parse_common_attribs attribs) in
    let r = List.flatten content in
    [ (Html.div ?a r : [> `Div ] Html.elt) ]

  let default_extension ~name _ attribs content =
    let s = string_of_extension name attribs content in
    [ Html.txt s ]

  let default_ni_extension = default_extension
end

module ReducedFlowBuilder = struct
  (* without image *)

  include FlowBuilder

  let img_elem _ _ _ =
    [ Html.em [ Html.txt "Images not enabled in this syntax" ] ]
end

module Reduced2FlowBuilder = struct
  (* no images, no titles, no tables, no lists, no subwikiboxes, no content, no
     objects *)

  include ReducedFlowBuilder

  let h1_elem = p_elem
  let h2_elem = p_elem
  let h3_elem = p_elem
  let h4_elem = p_elem
  let h5_elem = p_elem
  let h6_elem = p_elem
  let ul_elem _ _ = [ Html.em [ Html.txt "Lists not enabled in this syntax" ] ]
  let ol_elem _ _ = [ Html.em [ Html.txt "Lists not enabled in this syntax" ] ]
  let dl_elem _ _ = [ Html.em [ Html.txt "Lists not enabled in this syntax" ] ]

  let table_elem _ _ =
    [ Html.em [ Html.txt "Tables not enabled in this syntax" ] ]
end

module PhrasingBuilder = struct
  (* no images, no titles, no tables, no lists, no subwikiboxes, no content, no
     objects, no paragraph, no pre, ... *)

  include Reduced2FlowBuilder

  let p_elem _ (c : PhrasingTypes.text Html.elt list list) :
      PhrasingTypes.res_without_interactive Html.elt list =
    let l = List.map (fun x -> Html.totl (Html.toeltl x)) c in
    List.flatten l

  let pre_elem _ _ =
    [ Html.em [ Html.txt "Blocks of code not enabled in this syntax" ] ]

  let h1_elem = span_elem
  let h2_elem = span_elem
  let h3_elem = span_elem
  let h4_elem = span_elem
  let h5_elem = span_elem
  let h6_elem = span_elem
  let section_elem = span_elem

  let hr_elem _ =
    [ Html.em [ Html.txt "Horizontal rules not enabled in this syntax" ] ]

  let table_elem _ _ =
    [ Html.em [ Html.txt "Tables not enabled in this syntax" ] ]

  let ignore_a_elem_flow = ignore_a_elem_phrasing
end

module MenuBuilder = struct
  include FlowBuilder

  let nothing _ _ = []
  and nothing1 _ = []

  let strong_elem = strong_elem
  let em_elem = em_elem
  let monospace_elem = monospace_elem
  let underlined_elem = underlined_elem
  let linethrough_elem = linethrough_elem
  let subscripted_elem = subscripted_elem
  let superscripted_elem = superscripted_elem
  let a_elem_phrasing = a_elem_phrasing
  let a_elem_flow _ _ _ = []
  let make_href = menu_make_href
  let br_elem = nothing1
  let p_elem = nothing
  let pre_elem = nothing
  let section_elem = nothing
  let ul_elem = nothing
  let ol_elem = nothing
  let dl_elem = nothing
  let hr_elem = nothing1
  let table_elem = nothing
  let ignore_a_elem_flow _ _ _ = []
end

module ButtonBuilder = struct
  include FlowBuilder

  let forbid0 s =
    [ (Html.em [ Html.txt (s ^ " not enabled in buttons") ]
        : [ Html_types.button_content | `PCDATA ] Html.elt)
    ]

  let forbid1 s _ = forbid0 s
  let forbid2 s _ _ = forbid0 s
  let forbid3 s _ _ _ = forbid0 s
  let strong_elem = forbid2 "strong"
  let em_elem = forbid2 "em"
  let monospace_elem = forbid2 "monospace"
  let underlined_elem = forbid2 "underlined"
  let linethrough_elem = forbid2 "linethrough"
  let subscripted_elem = forbid2 "subscripted"
  let superscripted_elem = forbid2 "superscripted"
  let a_elem_phrasing = forbid3 "a_elem"
  let a_elem_flow = forbid3 "a_elem"
  let br_elem = forbid1 "br"
  let img_elem = forbid3 "img"
  let tt_elem = forbid2 "tt"
  let nbsp = forbid0 "nbsp"
  let endash = forbid0 "endash"
  let emdash = forbid0 "emdash"
  let p_elem = forbid2 "p_elem"
  let pre_elem = forbid2 "pre"
  let h1_elem = forbid2 "h1"
  let h2_elem = forbid2 "h2"
  let h3_elem = forbid2 "h3"
  let h4_elem = forbid2 "h4"
  let h5_elem = forbid2 "h5"
  let h6_elem = forbid2 "h6"
  let section_elem = forbid2 "section"
  let ul_elem = forbid2 "ul"
  let ol_elem = forbid2 "ol"
  let dl_elem = forbid2 "dl"
  let hr_elem = forbid1 "hr"
  let table_elem = forbid2 "table"
  let phrasing = forbid1 "phrasing"
  let ignore_a_elem_phrasing = forbid3 "a_elem"
  let ignore_a_elem_flow = forbid3 "a_elem"
end

(********************************)
(* Predefined builders.         *)

(* Default parser *)

module WikicreoleParser = MakeParser (struct
  include FlowTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include FlowBuilder
end)

let wikicreole_parser =
  (module WikicreoleParser : ExtParser
    with type res = WikicreoleParser.res
     and type res_without_interactive = WikicreoleParser.res_without_interactive
     and type link_content = WikicreoleParser.link_content)

(* Default flow parser but types as flow5_without_header_footer *)

module WikicreoleParserWithoutHeaderFooter = MakeParser (struct
  include FlowWithoutHeaderFooterTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include FlowBuilder
end)

let wikicreole_parser_without_header_footer =
  (module WikicreoleParserWithoutHeaderFooter : ExtParser
    with type res = WikicreoleParserWithoutHeaderFooter.res
     and type res_without_interactive = WikicreoleParserWithoutHeaderFooter
                                        .res_without_interactive
     and type link_content = WikicreoleParserWithoutHeaderFooter.link_content)

(* Reduced parsers. *)

module ReducedWikicreoleParser0 = MakeParser (struct
  include FlowTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include FlowBuilder
end)

module ReducedWikicreoleParser1 = MakeParser (struct
  include FlowTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include ReducedFlowBuilder
end)

module ReducedWikicreoleParser2 = MakeParser (struct
  include FlowTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include Reduced2FlowBuilder
end)

let reduced_wikicreole_parser0 =
  (module ReducedWikicreoleParser0 : ExtParser
    with type res = ReducedWikicreoleParser0.res
     and type res_without_interactive = ReducedWikicreoleParser0
                                        .res_without_interactive
     and type link_content = ReducedWikicreoleParser0.link_content)

let reduced_wikicreole_parser1 =
  (module ReducedWikicreoleParser1 : ExtParser
    with type res = ReducedWikicreoleParser1.res
     and type res_without_interactive = ReducedWikicreoleParser1
                                        .res_without_interactive
     and type link_content = ReducedWikicreoleParser1.link_content)

let reduced_wikicreole_parser2 =
  (module ReducedWikicreoleParser2 : ExtParser
    with type res = ReducedWikicreoleParser2.res
     and type res_without_interactive = ReducedWikicreoleParser2
                                        .res_without_interactive
     and type link_content = ReducedWikicreoleParser2.link_content)

(* Phrasing parser. *)

module PhrasingWikicreoleParser = MakeParser (struct
  include PhrasingTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include PhrasingBuilder
end)

let phrasing_wikicreole_parser =
  (module PhrasingWikicreoleParser : ExtParser
    with type res = PhrasingWikicreoleParser.res
     and type res_without_interactive = PhrasingWikicreoleParser
                                        .res_without_interactive
     and type link_content = PhrasingWikicreoleParser.link_content)

(* Menu builder *)

module MenuParser = MakeParser (struct
  include MenuTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list

  let phrasing x = (x : phrasing_without_interactive :> phrasing)

  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include MenuBuilder
end)

let menu_parser =
  (module MenuParser : ExtParser
    with type res = MenuParser.res
     and type res_without_interactive = MenuParser.res_without_interactive
     and type link_content = MenuParser.link_content)

(* Button builder *)

module ButtonParser = MakeParser (struct
  include ButtonTypes

  (* *)
  type flow = res Html.elt list
  type flow_without_interactive = res_without_interactive Html.elt list

  let flow x = (x : flow_without_interactive :> flow)

  type phrasing = text Html.elt list
  type phrasing_without_interactive = link_content Html.elt list
  type uo_list = list_item Html.elt list

  let list x = (x : uo_list :> flow_without_interactive)

  (* *)
  include ButtonBuilder
end)

let reduced_wikicreole_parser_button_content =
  (module ButtonParser : ExtParser
    with type res = [ Html_types.button_content | `PCDATA ]
     and type res_without_interactive = [ Html_types.button_content | `PCDATA ]
     and type link_content = [ Html_types.button_content | `PCDATA ])

(*************************)
(** Registering extension *)

type (+'flow_without_interactive
     , +'phrasing_without_interactive)
     non_interactive_simple_plugin =
  ( Wiki_widgets_interface.box_info
  , ('flow_without_interactive, 'phrasing_without_interactive) ni_plugin_content
  )
  Wikicreole.plugin

type (+'flow
     , +'flow_without_interactive
     , +'phrasing_without_interactive)
     interactive_simple_plugin =
  ( Wiki_widgets_interface.box_info
  , ( 'flow
    , 'flow_without_interactive
    , 'phrasing_without_interactive )
    plugin_content )
  Wikicreole.plugin

type +'without_interactive link_simple_plugin =
  ( Wiki_widgets_interface.box_info
  , href * Wikicreole.attribs * 'without_interactive Html.elt list )
  Wikicreole.plugin

let register_simple_extension (type a b c)
    ~(wp : (a, b, c) ext_wikicreole_parser) ~name ?preparser ?ni_plugin plugin =
  let module Parser = (val wp : ExtParser
                         with type res = a
                          and type res_without_interactive = b
                          and type link_content = c)
  in
  let open Parser in
  register_extension ~name ?preparser (SimplePlugin (plugin, ni_plugin))

(***** Registering to a group of parser. *)

let register_simple_flow_extension ~name ?(reduced = true) ?preparser
    (plugin :
      ( [< Html_types.flow5_without_interactive_header_footer ]
      , [< Html_types.phrasing_without_interactive ] )
      non_interactive_simple_plugin) =
  register_simple_extension ~name ?preparser ~wp:wikicreole_parser
    ~ni_plugin:(plugin :> WikicreoleParser.simple_ni_plugin)
    (plugin :> WikicreoleParser.simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:(plugin :> WikicreoleParserWithoutHeaderFooter.simple_ni_plugin)
    (plugin :> WikicreoleParserWithoutHeaderFooter.simple_plugin);
  if reduced
  then (
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser0
      ~ni_plugin:(plugin :> ReducedWikicreoleParser0.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser0.simple_plugin);
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser1
      ~ni_plugin:(plugin :> ReducedWikicreoleParser1.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser1.simple_plugin);
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser2
      ~ni_plugin:(plugin :> ReducedWikicreoleParser2.simple_ni_plugin)
      (plugin :> ReducedWikicreoleParser2.simple_plugin))

let register_interactive_simple_flow_extension ~name ?(reduced = true)
    ?preparser
    (plugin :
      ( Html_types.flow5_without_header_footer
      , Html_types.flow5_without_interactive_header_footer
      , Html_types.phrasing_without_interactive )
      interactive_simple_plugin) =
  register_simple_extension ~name ?preparser ~wp:wikicreole_parser
    (plugin :> WikicreoleParser.simple_plugin);
  register_simple_extension ~name ?preparser
    ~wp:wikicreole_parser_without_header_footer
    (plugin :> WikicreoleParserWithoutHeaderFooter.simple_plugin);
  if reduced
  then (
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser0
      (plugin
        : ( Html_types.flow5_without_header_footer
          , Html_types.flow5_without_interactive_header_footer
          , Html_types.phrasing_without_interactive )
          interactive_simple_plugin
        :> ReducedWikicreoleParser0.simple_plugin);
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser1
      (plugin :> ReducedWikicreoleParser1.simple_plugin);
    register_simple_extension ~name ?preparser ~wp:reduced_wikicreole_parser2
      (plugin :> ReducedWikicreoleParser2.simple_plugin))

let register_interactive_simple_flow_extension =
  (register_interactive_simple_flow_extension
    :    name:_
      -> ?reduced:_
      -> ?preparser:_
      -> ( Html_types.flow5_without_header_footer
         , Html_types.flow5_without_interactive_header_footer
         , Html_types.phrasing_without_interactive )
         interactive_simple_plugin
      -> unit
    :>    name:_
       -> ?reduced:_
       -> ?preparser:_
       -> ( [< Html_types.flow5_without_header_footer ]
          , [< Html_types.flow5_without_interactive_header_footer ]
          , [< Html_types.phrasing_without_interactive ] )
          interactive_simple_plugin
       -> unit)

let register_link_simple_flow_extension ~name ?reduced ?preparser plugin =
  let plugin wb attribs c = `Flow5_link (plugin wb attribs c) in
  register_interactive_simple_flow_extension ~name ?reduced ?preparser plugin

let register_simple_phrasing_extension ~name ?reduced ?preparser
    (plugin :
      ( Html_types.phrasing_without_interactive
      , Html_types.phrasing_without_interactive )
      non_interactive_simple_plugin) =
  register_simple_flow_extension ~name ?reduced ?preparser
    (plugin
      :> ( Html_types.flow5_without_interactive_header_footer
         , Html_types.phrasing_without_interactive )
         non_interactive_simple_plugin);
  register_simple_extension ~name ?preparser ~wp:phrasing_wikicreole_parser
    ~ni_plugin:(plugin :> PhrasingWikicreoleParser.simple_ni_plugin)
    (plugin :> PhrasingWikicreoleParser.simple_plugin)

let register_simple_phrasing_extension =
  (register_simple_phrasing_extension
    :    name:_
      -> ?reduced:_
      -> ?preparser:_
      -> ( Html_types.phrasing_without_interactive
         , Html_types.phrasing_without_interactive )
         non_interactive_simple_plugin
      -> unit
    :>    name:_
       -> ?reduced:_
       -> ?preparser:_
       -> ( [< Html_types.phrasing_without_interactive ]
          , [< Html_types.phrasing_without_interactive ] )
          non_interactive_simple_plugin
       -> unit)

let register_interactive_simple_phrasing_extension ~name ?reduced ?preparser
    (plugin :
      ( Html_types.phrasing
      , Html_types.phrasing_without_interactive
      , Html_types.phrasing_without_interactive )
      interactive_simple_plugin) =
  register_interactive_simple_flow_extension ~name ?reduced ?preparser
    (plugin
      :> ( Html_types.flow5_without_header_footer
         , Html_types.flow5_without_interactive_header_footer
         , Html_types.phrasing_without_interactive )
         interactive_simple_plugin);
  register_simple_extension ~name ?preparser ~wp:phrasing_wikicreole_parser
    (plugin :> PhrasingWikicreoleParser.simple_plugin)

let register_interactive_simple_phrasing_extension =
  (register_interactive_simple_phrasing_extension
    :    name:_
      -> ?reduced:_
      -> ?preparser:_
      -> ( Html_types.phrasing
         , Html_types.phrasing_without_interactive
         , Html_types.phrasing_without_interactive )
         interactive_simple_plugin
      -> unit
    :>    name:_
       -> ?reduced:_
       -> ?preparser:_
       -> ( [< Html_types.phrasing ]
          , [< Html_types.phrasing_without_interactive ]
          , [< Html_types.phrasing_without_interactive ] )
          interactive_simple_plugin
       -> unit)

let register_link_simple_phrasing_extension ~name ?reduced ?preparser plugin =
  let plugin wb attribs c = `Phrasing_link (plugin wb attribs c) in
  register_interactive_simple_flow_extension ~name ?reduced ?preparser plugin

(**** *)

type (-'content
     , +'flow_without_interactive
     , +'phrasing_without_interactive)
     wiki_plugin =
     Wiki_widgets_interface.box_info
  -> Wikicreole.attribs
  -> 'content Html.elt list option
  -> ( 'flow_without_interactive
     , 'phrasing_without_interactive )
     ni_plugin_content

let register_wiki_extension (type a b c a' b' c') ~wp ~name ~wp_rec ?preparser
    ?(context = fun bi _ -> bi) ?(ni_plugin : (_, _, _) wiki_plugin option)
    (plugin : (_, _, _) wiki_plugin) =
  let module Parser = (val wp : ExtParser
                         with type res = a
                          and type res_without_interactive = b
                          and type link_content = c)
  in
  let module Plugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'

    let wikiparser = wp_rec
    let update_context = context
    let plugin = (plugin :> rec_res Parser.wiki_plugin)

    let ni_plugin =
      (ni_plugin :> rec_res_without_interactive Parser.wiki_ni_plugin option)
  end in
  let open Parser in
  Parser.register_extension ~name ?preparser
    (WikiPlugin (module Plugin : WikiPlugin))

type (-'content
     , +'flow_without_interactive
     , +'phrasing_without_interactive)
     link_plugin =
     Wiki_widgets_interface.box_info
  -> Wikicreole.attribs
  -> 'content Html.elt list option
  -> ( 'flow_without_interactive
     , 'phrasing_without_interactive )
     link_plugin_content

let register_link_extension (type a b c a' b' c') ~wp ~name ~wp_rec ?preparser
    ?(context = fun bi _ -> bi) (plugin : (_, _, _) link_plugin) =
  let module Parser = (val wp : ExtParser
                         with type res = a
                          and type res_without_interactive = b
                          and type link_content = c)
  in
  let module Plugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'

    let wikiparser = wp_rec
    let update_context = context
    let plugin = plugin
  end in
  let open Parser in
  register_extension ~name ?preparser (LinkPlugin (module Plugin : LinkPlugin))

let register_raw_wiki_extension (type a b c a' b' c') ~wp ~name ~wp_rec
    ?preparser ?ni_plugin plugin =
  let module Parser = (val wp : ExtParser
                         with type res = a
                          and type res_without_interactive = b
                          and type link_content = c)
  in
  let open Parser in
  let module Plugin : RawWikiPlugin = struct
    type rec_res = a'
    type rec_res_without_interactive = b'
    type rec_link_content = c'

    let wikiparser = wp_rec
    let plugin = plugin
    let ni_plugin = ni_plugin
  end in
  register_extension ~name ?preparser
    (RawWikiPlugin (module Plugin : RawWikiPlugin))

type wiki_flow_pplugin =
  { fpp :
      'flow.
      ( 'flow
        Html_types.between_flow5_and_flow5_without_interactive_header_footer
      , 'flow
      , Html_types.phrasing_without_interactive )
      wiki_plugin
  }

let register_wiki_flow_extension ~name ?(reduced = true) ?preparser plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      ~ni_plugin:
        (plugin.fpp
          :> ( FlowTypes.res_without_interactive
             , FlowTypes.res_without_interactive
             , _ )
             wiki_plugin)
      (plugin.fpp :> (FlowTypes.res, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    ~ni_plugin:
      (plugin.fpp
        :> ( FlowWithoutHeaderFooterTypes.res_without_interactive
           , FlowWithoutHeaderFooterTypes.res_without_interactive
           , _ )
           wiki_plugin)
    (plugin.fpp
      :> ( FlowWithoutHeaderFooterTypes.res
         , FlowWithoutHeaderFooterTypes.res
         , _ )
         wiki_plugin);
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2)

type interactive_wiki_flow_pplugin =
  { ifpp :
      'flow 'flow_without_interactive.
      ( ( 'flow
        , 'flow_without_interactive )
        Html_types.between_flow5_and_flow5_without_header_footer
      , 'flow
      , Html_types.phrasing_without_interactive )
      wiki_plugin
  }

let register_interactive_wiki_flow_extension ~name ?(reduced = true) ?preparser
    plugin =
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec:wp ?preparser
      (plugin.ifpp :> (FlowTypes.res, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp:wikicreole_parser_without_header_footer
    ~wp_rec:wikicreole_parser_without_header_footer ?preparser
    (plugin.ifpp
      :> ( FlowWithoutHeaderFooterTypes.res
         , FlowWithoutHeaderFooterTypes.res
         , _ )
         wiki_plugin);
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2)

type link_wiki_flow_pplugin =
  { lfpp :
      'flow_without_interactive.
         Wiki_widgets_interface.box_info
      -> Wikicreole.attribs
      -> ([> Html_types.flow5_without_interactive_header_footer ]
          as
          'flow_without_interactive)
         Html.elt
         list
         option
      -> href * Wikicreole.attribs * 'flow_without_interactive Html.elt list
  }

let register_link_flow_extension ~name ?(reduced = true) ?preparser plugin =
  let plugin wb attribs c = `Flow5_link (plugin.lfpp wb attribs c) in
  let register wp =
    register_link_extension ~name ~wp ~wp_rec:wp ?preparser plugin
  in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2)

type wiki_phrasing_pplugin =
  { ppp :
      'phrasing 'phrasing_without_interactive.
      ( ( 'phrasing
        , 'phrasing_without_interactive )
        Html_types.between_phrasing_and_phrasing_without_interactive
      , 'phrasing
      , Html_types.phrasing_without_interactive )
      wiki_plugin
  }

let register_wiki_phrasing_extension ~name ?(reduced = true) ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name ~wp_rec ?preparser ~wp
      ~ni_plugin:
        (plugin.ppp
          : (FlowTypes.link_content, FlowTypes.link_content, _) wiki_plugin
          :> ( FlowTypes.link_content
             , FlowTypes.res_without_interactive
             , _ )
             wiki_plugin)
      (plugin.ppp
        : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
        :> (FlowTypes.text, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
    ~ni_plugin:
      (plugin.ppp
        : (FlowTypes.link_content, FlowTypes.link_content, _) wiki_plugin
        :> ( _
           , FlowWithoutHeaderFooterTypes.res_without_interactive
           , _ )
           wiki_plugin)
    (plugin.ppp
      : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
      :> (_, FlowWithoutHeaderFooterTypes.res, _) wiki_plugin);
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2);
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser ~ni_plugin:plugin.ppp plugin.ppp

let register_interactive_wiki_phrasing_extension ~name ?(reduced = true)
    ?preparser plugin =
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_wiki_extension ~name ~wp ~wp_rec ?preparser
      (plugin.ppp
        : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
        :> (_, FlowTypes.res, _) wiki_plugin)
  in
  register wikicreole_parser;
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:wikicreole_parser_without_header_footer
    (plugin.ppp
      : (FlowTypes.text, FlowTypes.text, _) wiki_plugin
      :> (_, FlowWithoutHeaderFooterTypes.res, _) wiki_plugin);
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2);
  register_wiki_extension ~name ~wp_rec ?preparser
    ~wp:phrasing_wikicreole_parser plugin.ppp

type link_wiki_phrasing_pplugin =
     Wiki_widgets_interface.box_info
  -> Wikicreole.attribs
  -> Html_types.phrasing_without_interactive Html.elt list option
  -> href
     * Wikicreole.attribs
     * Html_types.phrasing_without_interactive Html.elt list

let register_link_phrasing_extension ~name ?(reduced = true) ?preparser
    (plugin : link_wiki_phrasing_pplugin) =
  let plugin wb attribs c = `Phrasing_link (plugin wb attribs c) in
  let wp_rec = phrasing_wikicreole_parser in
  let register wp =
    register_link_extension ~name ~wp ~wp_rec ?preparser plugin
  in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  if reduced
  then (
    register reduced_wikicreole_parser0;
    register reduced_wikicreole_parser1;
    register reduced_wikicreole_parser2);
  register phrasing_wikicreole_parser

(* Extensions: div; aside; article; nav; section; header; footer *)

let f_block make _ args content =
  `Flow5
    (let a = Some (parse_common_attribs args) in
     match content with
     | None -> [ make ?a [] ]
     | Some content ->
       let content = content in
       [ make ?a content ])

let () =
  let add_divs wp wp_rec =
    List.iter
      (fun (name, make, make') ->
        (* FIXME it won't type without duplicating the 'make' argument... *)
        register_wiki_extension ~wp ~name ~wp_rec ~ni_plugin:(f_block make')
          (f_block make))
      [ ("div", Html.div, Html.div)
      ; ("aside", Html.aside, Html.aside)
      ; ("article", Html.article, Html.article)
      ; ("nav", Html.nav, Html.nav)
      ; ("section", Html.section, Html.section)
      ]
  in
  add_divs wikicreole_parser wikicreole_parser;
  add_divs wikicreole_parser_without_header_footer wikicreole_parser;
  add_divs reduced_wikicreole_parser0 reduced_wikicreole_parser0;
  add_divs reduced_wikicreole_parser1 reduced_wikicreole_parser1;
  add_divs reduced_wikicreole_parser2 reduced_wikicreole_parser2

let () =
  List.iter
    (fun (name, make, make') ->
      (* FIXME it won't type without duplicating the 'make' argument... *)
      register_wiki_extension ~name ~wp:wikicreole_parser
        ~context:(fun bi _ -> { bi with bi_sectioning = false })
        ~wp_rec:wikicreole_parser_without_header_footer
        ~ni_plugin:(f_block make') (f_block make))
    [ ("header", Html.header, Html.header)
    ; ("footer", Html.footer, Html.footer)
    ]

(* pre *)

let f_pre _ args content =
  `Flow5
    (let content =
       match content with
       | None -> []
       | Some c -> (c :> Html_types.pre_content Html.elt list)
     in
     let a = Some (parse_common_attribs args) in
     [ Html.pre ?a content ])

let () =
  let register wp =
    register_wiki_extension ~wp ~wp_rec:phrasing_wikicreole_parser ~name:"pre"
      ~ni_plugin:f_pre f_pre
  in
  register wikicreole_parser;
  register wikicreole_parser_without_header_footer;
  register reduced_wikicreole_parser0;
  register reduced_wikicreole_parser1;
  register reduced_wikicreole_parser2

(* span *)

let f_span _ args content =
  `Phrasing_without_interactive
    (let content =
       match content with
       | None -> []
       | Some c -> (c :> Html_types.phrasing Html.elt list)
     in
     let a = Some (parse_common_attribs args) in
     [ (Html.span ?a content : 'a Html.elt) ])

let () = register_wiki_phrasing_extension ~name:"span" { ppp = f_span }

(* Empty (comment) *)

let f_empty _bi _args _c = `Flow5 []
let () = register_simple_phrasing_extension ~name:"" f_empty

(* Templating *)

let () =
  let f_content bi _ _ = `Flow5 bi.bi_content in
  let nope _ _ _ = `Flow5 (FlowBuilder.error "content is interactive") in
  let wp = wikicreole_parser in
  register_wiki_extension ~name:"content" ~wp ~wp_rec:wp
    ~context:(fun bi _ -> bi)
    ~ni_plugin:nope f_content

let () =
  let f_title bi _ _ = `Phrasing_without_interactive [ Html.txt bi.bi_title ] in
  register_simple_phrasing_extension ~name:"title" f_title

let compile_with_content content_text text =
  let par = cast_wp wikicreole_parser in
  let bi_with_content c =
    Wiki_widgets_interface.
      { bi_page = Site ""
      ; bi_sectioning = false
      ; bi_add_link = ignore
      ; bi_content = c
      ; bi_title = ""
      }
  in
  let c = xml_of_wiki par (bi_with_content @@ []) content_text in
  xml_of_wiki par (bi_with_content c) text

let compile text = compile_with_content "" text

let () =
  let f_include _ args _ =
    let file =
      match (List.assoc_opt "wiki" args, List.assoc_opt "template" args) with
      | None, None ->
        failwith "include: required attribute \"wiki\" or \"template\" missing"
      | Some _, Some _ ->
        failwith
          "include: conflicting attributes \"wiki\" and \"template\" both \
           provided"
      | None, Some wiki -> (
        match (Global.options ()).template with
        | Some template ->
          let template_dir = Filename.dirname template in
          Utils.Operators.(template_dir +/+ wiki)
        | None ->
          failwith "include: extension requires --template to be provided")
      | Some wiki, None ->
        let current_dir = Global.current_file () |> Filename.dirname in
        Utils.Operators.(current_dir +/+ wiki)
    in
    file |> Utils.read_file |> compile |> fun c -> `Flow5 c
  in
  let wp = wikicreole_parser in
  register_wiki_extension ~name:"include" ~wp ~wp_rec:wp
    ~context:(fun bi _ -> bi)
    f_include
