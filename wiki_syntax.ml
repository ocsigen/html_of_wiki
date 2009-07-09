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
(**
   Pretty print wiki to OcamlDuce
   @author Vincent Balat
   @author Boris Yakobowski
*)

open Wiki_types
open Wiki_widgets_interface
let (>>=) = Lwt.bind


let class_wikibox wb = Printf.sprintf "wikiboxcontent%s" (string_of_wikibox wb)


let string_of_extension name args content =
  "<<"^name^
    (List.fold_left
       (fun beg (n, v) -> beg^" "^n^"='"^v^"'") "" args)^
    (match content with
       | None -> "" 
       | Some content -> "|"^content)^">>"



(***)
let make_string s = Lwt.return (Ocamlduce.Utf8.make s)

let element (c : Xhtmltypes_duce.inlines Lwt.t list) = 
  Lwt_util.map_serial (fun x -> x) c >>= fun c ->
  Lwt.return {{ (map {: c :} with i -> i) }}

let element2 (c : {{ [ Xhtmltypes_duce.a_content* ] }} list) = 
  {{ (map {: c :} with i -> i) }}


(* I don't know how to do a function for 
    try
      let c = Ocamlduce.Utf8.make (List.assoc name attribs) in
      {{ {...=c} }}
    with Not_found -> atts
that typechecks with ocamlduce ... ?
*)
let parse_common_attribs attribs =
  let at1 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc "class" attribs) in
      {{ {class=c} }}
    with Not_found -> {{ {} }}
  and at2 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "id" attribs) in
      {{ {id=c} }}
    with Not_found -> {{ {} }}
  in
  ({{ at1++at2 }} : Xhtmltypes_duce.coreattrs)

let parse_table_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "border" attribs) in
      {{ {border=c} }}
    with Not_found -> {{ {} }}
  and at2 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "cellpadding" attribs) in
      {{ {cellpadding=c} }}
    with Not_found -> {{ {} }}
  and at3 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "cellspacing" attribs) in
      {{ {cellspacing=c} }}
    with Not_found -> {{ {} }}
(*  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "frame" attribs) in
      {{ {frame=c}++atts }}
    with Not_found -> atts
  in
  let atts =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "rules" attribs) in
      {{ {rules=c}++atts }}
    with Not_found -> atts
  in*)
  and at4 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "summary" attribs) in
      {{ {summary=c} }}
    with Not_found -> {{ {} }}
  and at5 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "width" attribs) in
      {{ {width=c} }}
    with Not_found -> {{ {} }}
  in
  ({{ atts++at1++at2++at3++at4++at5 }} : Xhtmltypes_duce.table_attrs)

let parse_valign_attrib attribs : {{ { valign =? Xhtmltypes_duce.valign } }} =
  try
    let c = List.assoc "valign" attribs in
    if c="top"
    then {{ {valign="top"} }}
    else
    if c="middle"
    then {{ {valign="middle"} }}
    else
    if c="bottom"
    then {{ {valign="bottom"} }}
    else
    if c="baseline"
    then {{ {valign="baseline"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_align_attrib attribs : {{ { align =? Xhtmltypes_duce.align } }} =
  try
    let c = List.assoc "align" attribs in
    if c="left"
    then {{ {align="left"} }}
    else
    if c="center"
    then {{ {align="center"} }}
    else
    if c="right"
    then {{ {align="right"} }}
    else
    if c="justify"
    then {{ {align="justify"} }}
    else
    if c="char"
    then {{ {align="char"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_scope_attrib attribs : {{ { scope =? Xhtmltypes_duce.scope } }} =
  try
    let c = List.assoc "scope" attribs in
    if c="row"
    then {{ {scope="row"} }}
    else
    if c="col"
    then {{ {scope="col"} }}
    else
    if c="rowgroup"
    then {{ {scope="rowgroup"} }}
    else
    if c="colgroup"
    then {{ {scope="colgroup"} }}
    else {{ {} }}
  with Not_found -> {{ {} }}

let parse_table_row_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "char" attribs) in
      {{ {char=c} }}
    with Not_found -> {{ {} }}
    and at2 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "charoff" attribs) in
      {{ {charoff=c} }}
    with Not_found -> {{ {} }}
  and atts2 = parse_valign_attrib attribs in
  let atts3 = parse_align_attrib attribs in
  ({{ atts++at1++at2++atts2++atts3++atts }} : Xhtmltypes_duce.align_attrs)

let parse_table_cell_attribs attribs =
  let atts = parse_common_attribs attribs
  and at1 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "char" attribs) in
      {{ {char=c} }}
    with Not_found -> {{ {} }}
  and at2  =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "charoff" attribs) in
      {{ {charoff=c} }}
    with Not_found -> {{ {} }}
  and at3  =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "abbr" attribs) in
      {{ {abbr=c} }}
    with Not_found -> {{ {} }}
  and at4 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "axis" attribs) in
      {{ {axis=c} }}
    with Not_found -> {{ {} }}
  and at5 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "colspan" attribs) in
      {{ {colspan=c} }}
    with Not_found -> {{ {} }}
  and at6 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "headers" attribs) in
      {{ {headers=c} }}
    with Not_found -> {{ {} }}
  and at7 =
    try
      let c = Ocamlduce.Utf8.make (List.assoc  "rowspan" attribs) in
      {{ {rowspan=c} }}
    with Not_found -> {{ {} }}
  and atts2 = parse_valign_attrib attribs
  and atts3 = parse_align_attrib attribs
  and atts4 = parse_scope_attrib attribs in
  ({{ atts++at1++at2++at3++at4++at5++at6++at7++atts2++atts3++atts4 }}
     : Xhtmltypes_duce.thd_attribs)

let list_builder = function
  | [] -> Lwt.return {{ [ <li>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (c, 
             (l : Xhtmltypes_duce.flows Lwt.t option),
             attribs) =
        let atts = parse_common_attribs attribs in
        element c >>= fun r ->
        (match l with
          | Some v -> v >>= fun v -> Lwt.return v
          | None -> Lwt.return {{ [] }}) >>= fun l ->
        Lwt.return
          {{ <li (atts)>[ !r
                   !l ] }}
      in
      f a >>= fun r ->
      Lwt_util.map_serial f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let descr_builder = function
  | [] -> Lwt.return {{ [ <dt>[] ] }} (*VVV ??? *)
  | a::l ->
      let f (istitle, d, attribs) =
        let atts = parse_common_attribs attribs in
        element d >>= fun d ->
        if istitle
        then Lwt.return {{ <dt (atts)>[ !d ] }}
        else Lwt.return {{ <dd (atts)>[ !d ] }}
      in
      f a >>= fun r ->
      Lwt_util.map_serial f l >>= fun l ->
      Lwt.return {{ [ r !{: l :} ] }}

let inline (x : Xhtmltypes_duce.a_content)
    : Xhtmltypes_duce.inlines
    = {{ {: [ x ] :} }}

let link_regexp = Netstring_pcre.regexp "(http\\+|https\\+)?([a-z|A-Z|0-9]+)(\\((.*)\\))?:(.*)"

type force_https = bool option

type link_kind =
  | Absolute of string
  | Page of string * force_https
  | Wiki_page of Wiki_types.wiki * string * force_https
  | Site of string * force_https

let link_kind addr = 
  match Netstring_pcre.string_match link_regexp addr 0 with
    | None -> Page (addr, None)
    | Some result ->
        let forceproto = 
          try
            if (Netstring_pcre.matched_group result 1 addr) = "https+"
            then Some true
            else Some false
          with Not_found -> None
        in
        let proto = Netstring_pcre.matched_group result 2 addr in
        if proto = "wiki"
        then
          let page = Netstring_pcre.matched_group result 5 addr in
          try
            let wikinum = Netstring_pcre.matched_group result 4 addr in
            Wiki_page ((Wiki_types.wiki_of_sql (Int32.of_string wikinum)),
                       page,
                       forceproto)
          with Failure _ | Not_found -> Page (page, forceproto)
        else if proto = "site"
        then
          let page = Netstring_pcre.matched_group result 5 addr in
          Site (page, forceproto)
        else Absolute addr


type syntax_extension =
    (Wiki_widgets_interface.box_info,
     Xhtmltypes_duce.flows Lwt.t,
     Eliom_duce.Blocks.a_content_elt_list Lwt.t)
   Wikicreole.plugin


type 'a plugin_hash = (string, 'a) Hashtbl.t

(* A wikicreole_parser is essentially a [Wikicreole.builder] object
   (contained in the field [builder] but easily extensible. That is,
   the fields [plugin] and [plugin_action] of [Wikicreole.builder],
   which are supposed to be functions, are here represented as
   association tables. Thus, it becomes easy (and, more importantly,
   not costly) to add extensions. When a [wikicreole_parser] must be
   used as a builder, we call the function [builder_from_wikicreole_parser]
   (defined below), which updates the field [plugin] of the builder.
   (This supposes that the builder is used to parse something. When
   used as a preparser, the field [plugin_assoc] is also overridden
   using a proper function. See [preparse_extension] below.
*)
type wikicreole_parser = {
  builder: (Xhtmltypes_duce.flows Lwt.t,
            Xhtmltypes_duce.inlines Lwt.t,
            {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t,
            box_info,
            Eliom_sessions.server_params)
    Wikicreole.builder;

  plugin_assoc: (bool * syntax_extension) plugin_hash;

  plugin_action_assoc:
     ((Eliom_sessions.server_params * Wiki_types.wikibox,
       string option Lwt.t)
        Wikicreole.plugin_args)
    plugin_hash;
}

let builder_from_wikicreole_parser parser =
  { parser.builder with
      Wikicreole.plugin =
      (fun name ->
         try Hashtbl.find parser.plugin_assoc name
         with Not_found ->
           (false,
            (fun _ _ _ ->
               Wikicreole.A_content
                 (Lwt.return
                    {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                    !{: name :} ] ] ] }})))
      );
  }

let copy_parser wp = {
  wp with
    plugin_assoc = Hashtbl.copy wp.plugin_assoc;
    plugin_action_assoc = Hashtbl.copy wp.plugin_action_assoc;
}


let add_preparser_extension ~wp ~name f =
  Hashtbl.add wp.plugin_action_assoc name f


let preparse_extension
    wp (sp, wb : Eliom_sessions.server_params * Wiki_types.wikibox)
    content =
  let subst = ref [] in
  let plugin_action name start end_ params args content =
    subst := (start,
              end_,
              (try
                 (Hashtbl.find wp.plugin_action_assoc)
                   name params args content
               with Not_found -> Lwt.return None))::!subst
  and nothing _ _ = ()
  and nothing1 _ = ()
  in
  (* This builder does essentially nothing, except on extensions
     where it calls plugin_action *)
  let preparse_builder = {
    Wikicreole.chars = nothing1;
    strong_elem = nothing;
    em_elem = nothing;
    a_elem = (fun _ _ _ _ -> ());
    make_href = (fun _ _ a fragment -> match fragment with 
                   | None -> a
                   | Some f -> a ^"#"^f);
    br_elem = nothing1;
    img_elem = (fun _ _ _ -> ());
    tt_elem = nothing;
    monospace_elem = nothing;
    underlined_elem = nothing;
    linethrough_elem = nothing;
    subscripted_elem = nothing;
    superscripted_elem = nothing;
    nbsp = ();
    p_elem = nothing;
    pre_elem = nothing;
    h1_elem = nothing;
    h2_elem = nothing;
    h3_elem = nothing;
    h4_elem = nothing;
    h5_elem = nothing;
    h6_elem = nothing;
    ul_elem = nothing;
    ol_elem = nothing;
    dl_elem = nothing;
    hr_elem = nothing1;
    table_elem = nothing;
    inline = nothing1;
    plugin =
      (fun name ->
         let wiki_content =
           try fst (Hashtbl.find wp.plugin_assoc name)
           with Not_found -> false
         in (wiki_content, (fun _ _ _ -> Wikicreole.A_content ())));
    plugin_action = plugin_action;
    error = nothing1;
  } in
  Wikicreole.from_string sp (sp, wb) preparse_builder content
  >>= fun (_ : unit list) ->
  let buf = Buffer.create 1024 in
  Lwt_util.fold_left
    (fun pos (start, end_, replacement) ->
       replacement >>= function
         | None -> Lwt.return pos;
         | Some replacement ->
             Buffer.add_substring buf content pos (start - pos);
             Buffer.add_string buf replacement;
             Lwt.return end_
    )
    0
    (List.rev !subst)
  >>= fun pos ->
  let l = String.length content in
  if pos < l 
  then Buffer.add_substring buf content pos (l - pos);
  Lwt.return (Buffer.contents buf)

let site_url_syntax =
  {
    Neturl.url_enable_scheme = Neturl.Url_part_not_recognized;
    Neturl.url_enable_user = Neturl.Url_part_not_recognized;
    Neturl.url_enable_user_param = Neturl.Url_part_not_recognized;
    Neturl.url_enable_password = Neturl.Url_part_not_recognized;
    Neturl.url_enable_host = Neturl.Url_part_not_recognized;
    Neturl.url_enable_port = Neturl.Url_part_not_recognized;
    Neturl.url_enable_path = Neturl.Url_part_allowed;
    Neturl.url_enable_param = Neturl.Url_part_not_recognized;
    Neturl.url_enable_query = Neturl.Url_part_allowed;
    Neturl.url_enable_fragment = Neturl.Url_part_allowed;
    Neturl.url_enable_other = Neturl.Url_part_not_recognized;
    Neturl.url_accepts_8bits = true;
    Neturl.url_is_valid = (fun _ -> true);
    Neturl.url_enable_relative = true;
  }

let make_href sp bi addr fragment =
  let aux ~fragment https wiki page =
    match Wiki_self_services.find_servpage wiki with
      | Some servpage ->
          let addr =
            Ocsigen_lib.remove_slash_at_beginning
              (Neturl.split_path page)
          in
          Eliom_predefmod.Xhtml.make_string_uri ?https
            ?fragment ~service:servpage ~sp addr
      | None -> "malformed link" (*VVV ??? *)
  in
  match addr with
    | Page (page, forceproto) ->
        let wiki = bi.Wiki_widgets_interface.bi_wiki in
        aux ~fragment forceproto wiki page
    | Absolute addr -> (match fragment with
                          | None -> addr
                          | Some fragment -> addr^"#"^fragment)
    | Wiki_page (wiki, page, forceproto) -> aux ~fragment forceproto wiki page
    | Site (href, forceproto) ->
        try
          let url = Neturl.url_of_string site_url_syntax href in
          let path = Neturl.url_path url in
          let path =
            (Eliom_sessions.get_site_dir sp) @
              (Ocsigen_lib.remove_slash_at_beginning path)
          in
          match forceproto with
            | None ->
                let path =
                  Eliom_mkforms.reconstruct_relative_url_path
                    (Eliom_sessions.get_original_full_path ~sp)
                    path
                in
                Neturl.string_of_url (Neturl.modify_url ?fragment ~path url)
            | Some https -> 
                (Eliom_predefmod.Xhtml.make_proto_prefix ~sp https)^
                  (Neturl.string_of_url (Neturl.modify_url ?fragment ~path url))
        with Neturl.Malformed_URL -> 
          "malformed link"

let default_builder =
  { Wikicreole.chars = make_string;
    strong_elem = (fun attribs a -> 
                       let atts = parse_common_attribs attribs in
                       element a >>= fun r ->
                       Lwt.return {{ [<strong (atts)>r ] }});
    em_elem = (fun attribs a -> 
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<em (atts)>r] }});
    monospace_elem = (fun attribs a -> 
                          let atts = parse_common_attribs attribs in
                          element a >>= fun r ->
                            Lwt.return {{ [<tt (atts)>r] }});
    underlined_elem = (fun attribs a -> 
                           let atts = parse_common_attribs attribs in
                           element a >>= fun r ->
                           Lwt.return {{ [<span ({class="underlined"} ++
                                             atts)>r] }});
    linethrough_elem = (fun attribs a -> 
                           let atts = parse_common_attribs attribs in
                           element a >>= fun r ->
                           Lwt.return {{ [<span ({class="linethrough"} ++
                                             atts)>r] }});
    subscripted_elem = (fun attribs a -> 
                            let atts = parse_common_attribs attribs in
                            element a >>= fun r ->
                              Lwt.return {{ [<sub (atts)>r] }});
    superscripted_elem = (fun attribs a -> 
                              let atts = parse_common_attribs attribs in
                              element a >>= fun r ->
                                Lwt.return {{ [<sup (atts)>r] }});
    a_elem =
      (fun attribs _sp addr 
         (c : {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t list) -> 
           let atts = parse_common_attribs attribs in
           Lwt_util.map_serial (fun x -> x) c >>= fun c ->
           Lwt.return
             {{ [ <a ({href={: Ocamlduce.Utf8.make addr :}}++atts)>{: element2 c :} ] }});
    make_href = (fun a b c fragment -> make_href a b (link_kind c) fragment);
    br_elem = (fun attribs -> 
                   let atts = parse_common_attribs attribs in
                   Lwt.return {{ [<br (atts)>[]] }});
    img_elem =
      (fun attribs addr alt -> 
         let atts = parse_common_attribs attribs in
         Lwt.return 
           {{ [<img
                  ({src={: Ocamlduce.Utf8.make addr :} 
                    alt={: Ocamlduce.Utf8.make alt :}}
                   ++
                    atts)
                  >[] ] }});
    tt_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<tt (atts)>r ] }});
    nbsp = Lwt.return {{" "}};
    p_elem = (fun attribs a -> 
                  let atts = parse_common_attribs attribs in
                  element a >>= fun r ->
                  Lwt.return {{ [<p (atts)>r] }});
    pre_elem = (fun attribs a ->
       let atts = parse_common_attribs attribs in
       Lwt.return
         {{ [<pre (atts)>{:Ocamlduce.Utf8.make (String.concat "" a):}] }});
    h1_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h1 (atts)>r] }});
    h2_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h2 (atts)>r] }});
    h3_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h3 (atts)>r] }});
    h4_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h4 (atts)>r] }});
    h5_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h5 (atts)>r] }});
    h6_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   element a >>= fun r ->
                   Lwt.return {{ [<h6 (atts)>r] }});
    ul_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ul (atts)>r] }});
    ol_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   list_builder a >>= fun r ->
                   Lwt.return {{ [<ol (atts)>r] }});
    dl_elem = (fun attribs a ->
                   let atts = parse_common_attribs attribs in
                   descr_builder a >>= fun r ->
                   Lwt.return {{ [<dl (atts)>r] }});
    hr_elem = (fun attribs -> 
                   let atts = parse_common_attribs attribs in
                   Lwt.return {{ [<hr (atts)>[]] }});
    table_elem =
      (fun attribs l ->
         let atts = parse_table_attribs attribs in
         match l with
         | [] -> Lwt.return {{ [] }}
         | row::rows ->
             let f (h, attribs, c) =
               let atts = parse_table_cell_attribs attribs in
               element c >>= fun r ->
               Lwt.return
                 (if h 
                 then {{ <th (atts)>r }}
                 else {{ <td (atts)>r }})
             in
             let f2 (row, attribs) = match row with
               | [] -> Lwt.return {{ <tr>[<td>[]] }} (*VVV ??? *)
               | a::l -> 
                   let atts = parse_table_row_attribs attribs in
                   f a >>= fun r ->
                   Lwt_util.map_serial f l >>= fun l ->
                   Lwt.return {{ <tr (atts)>[ r !{: l :} ] }}
             in
             f2 row >>= fun row ->
             Lwt_util.map_serial f2 rows >>= fun rows ->
             Lwt.return {{ [<table (atts)>[<tbody>[ row !{: rows :} ] ] ] }});
    inline = (fun x -> x >>= fun x -> Lwt.return x);
    plugin = (fun name ->
                (false,
                 (fun _ _ _ ->
                    Wikicreole.A_content 
                      (Lwt.return
                         {{ [ <b>[<i>[ 'Wiki error: Unknown extension '
                                         !{: name :} ] ] ] }})))
      );
    plugin_action = (fun _ _ _ _ _ _ -> ());
    error = (fun s -> Lwt.return {{ [ <b>{: s :} ] }});
  }

let wikicreole_parser = {
  builder = default_builder;
  plugin_assoc = Hashtbl.create 17;
  plugin_action_assoc = Hashtbl.create 17;
}

let xml_of_wiki wp bi s =
  Wikicreole.from_string bi.Wiki_widgets_interface.bi_sp bi
       (builder_from_wikicreole_parser wp) s
  >>= fun l ->
  Lwt_util.map_serial (fun x -> x) l >>= fun r ->
  Lwt.return {{ (map {: r :} with i -> i) }}

let wikicreole_content_type = 
  Wiki_models.register_wiki_parser "wikicreole" 
    (preparse_extension wikicreole_parser)
    (xml_of_wiki wikicreole_parser)

let inline_of_wiki builder bi s : Xhtmltypes_duce.inlines Lwt.t =
  Wikicreole.from_string bi.Wiki_widgets_interface.bi_sp bi
    (builder_from_wikicreole_parser builder) s >>= function
    | [] -> Lwt.return {{ [] }}
    | a::_ -> a >>= (function
                       | {{ [ <p>l _* ] }} -> Lwt.return l
(*VVV What can I do with trailing data? *)
                       | {{ _ }} -> Lwt.return {{ [ <b>"error" ] }})

let wikicreole_inline_content_type container = 
  Wiki_models.register_wiki_parser "wikicreole_inline"
    (preparse_extension wikicreole_parser)
    (fun bi s -> 
       inline_of_wiki wikicreole_parser bi s >>= fun r ->
       Lwt.return (container r)
    )

let a_content_of_wiki builder bi s =
  inline_of_wiki builder bi s >>= fun r ->
  Lwt.return
    {{ map r with
         |  <a (Xhtmltypes_duce.a_attrs)>l -> l
         | p -> [p] }}




(*********************************************************************)

let add_extension ~wp ~name ?(wiki_content=true) f =
  Hashtbl.add wp.plugin_assoc name (wiki_content, f);
  if wiki_content then
    Hashtbl.add wp.plugin_action_assoc name
      (fun (sp, wb) args -> function
         | None -> Lwt.return None
         | Some c ->
             preparse_extension wp (sp, wb) c >>= fun c ->
             Lwt.return (Some (string_of_extension name args (Some c)))
      )


let () =
  let add_extension_aux = add_extension ~wp:wikicreole_parser in

  add_extension_aux ~name:"div" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.Block 
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          xml_of_wiki wikicreole_parser bi content >>= fun content ->
          let classe = 
            try
              let a = List.assoc "class" args in
              {{ { class={: a :} } }} 
            with Not_found -> {{ {} }} 
          in
          let id = 
            try
              let a = List.assoc "id" args in
              {{ { id={: a :} } }} 
            with Not_found -> {{ {} }} 
          in
          Lwt.return 
            {{ [ <div (classe ++ id) >content ] }})
    );


  add_extension_aux ~name:"span" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.A_content
         (let content = match c with
            | Some c -> c
            | None -> ""
          in
          inline_of_wiki wikicreole_parser bi content >>= fun content ->
          let classe = 
            try
              let a = List.assoc "class" args in
              {{ { class={: a :} } }} 
            with Not_found -> {{ {} }} 
          in
          let id = 
            try
              let a = List.assoc "id" args in
              {{ { id={: a :} } }} 
            with Not_found -> {{ {} }} 
          in
          Lwt.return 
            {{ [ <span (classe ++ id) >content ] }}
         )
    );

  add_extension_aux ~name:"wikiname" ~wiki_content:true
    (fun bi _ _ ->
       Wikicreole.A_content
         (let wid = bi.Wiki_widgets_interface.bi_wiki in
          Wiki_sql.get_wiki_info_by_id wid
          >>= fun wiki_info ->
          Lwt.return {{ {: Ocamlduce.Utf8.make (wiki_info.wiki_descr) :} }})
    );


  add_extension_aux ~name:"raw" ~wiki_content:false
    (fun _ args content ->
       Wikicreole.A_content
         (let s = string_of_extension "raw" args content in
          Lwt.return {{ [ <b>{: s :} ] }}));

  add_extension_aux ~name:"" ~wiki_content:false
    (fun _ _ _ ->
       Wikicreole.A_content (Lwt.return {{ [] }})
    );

  add_extension_aux ~name:"content"
    (fun bi args _ ->
       Wikicreole.Block
         (let classe, classe' =
            try
              let a = List.assoc "class" args in
              {{ { class={: a :} } }}, a
            with Not_found -> {{ {} }}, ""
          and id =
            try
              let a = List.assoc "id" args in
              {{ { id={: a :} } }}
            with Not_found -> {{ {} }}
          in
          bi.Wiki_widgets_interface.bi_subbox bi.bi_menu_style >>= function
            | None -> Lwt.return {{ [ <div (classe ++ id) >
                                        [<strong>[<em>"<<content>>"]]] }}
            | Some (wb, subbox) ->
                let classe = match wb with
                  | None -> classe
                  | Some wb ->
                      let cl = classe' ^ " " ^ class_wikibox wb in
                      {{ { class={: cl :}  } }}
                in
                Lwt.return {{ [ <div (classe ++ id) >subbox ] }}
         )
    );

  add_extension_aux ~name:"menu"
    (fun bi args _ ->
       let wiki_id = bi.Wiki_widgets_interface.bi_wiki in
       Wikicreole.Block
         (let classe = 
            let c =
              "wikimenu"^
                (try
                   " "^(List.assoc "class" args)
                 with Not_found -> "")
            in {{ { class={: c :} } }} 
          in
          let id = 
            try
              let a = List.assoc "id" args in
              {{ { id={: a :} } }} 
            with Not_found -> {{ {} }} 
          in
          let f ?classe s =
            let link, text = 
              try 
                Ocsigen_lib.sep '|' s 
              with Not_found -> s, s
            in
            Wiki_sql.get_wiki_info_by_id wiki_id >>= fun wiki_info ->
            a_content_of_wiki wikicreole_parser bi text >>= fun text2 ->
            let b = 
              match wiki_info.Wiki_types.wiki_pages with
                | Some dir ->
                    Eliom_sessions.get_current_sub_path_string
                      bi.Wiki_widgets_interface.bi_sp = dir^"/"^link
                | None -> false
            in
            if b
            then 
              let classe = match classe with
                | None -> {{ { class="wikimenu_current" } }}
                | Some c -> 
                    let c = Ocamlduce.Utf8.make ("wikimenu_current "^c) in
                    {{ { class=c } }}
              in
              Lwt.return {{ <li (classe)>text2}}
            else
              let href = 
                make_href bi.Wiki_widgets_interface.bi_sp bi 
                  (link_kind link) None in
              let link2 = Ocamlduce.Utf8.make href in
              let classe = match classe with
                | None -> {{ {} }}
                | Some c -> 
                    let c = Ocamlduce.Utf8.make c in
                    {{ { class=c } }}
              in
              Lwt.return {{ <li (classe)>[<a href=link2>text2]}}
          in
          let rec mapf = function
            | [] -> Lwt.return []
            | [a] -> f ~classe:"wikimenu_last" a >>= fun b -> Lwt.return [b]
              | a::ll -> f a >>= fun b -> mapf ll >>= fun l -> Lwt.return (b::l)
          in
          match
            List.rev
              (List.fold_left
                 (fun beg (n, v) -> if n="item" then v::beg else beg)
                 [] args)
          with
            | [] -> Lwt.return {: [] :}
            | [a] ->  
                f ~classe:"wikimenu_first wikimenu_last" a >>= fun first ->
                  Lwt.return {{ [ <ul (classe ++ id) >[ {: first :} ] ] }}
                | a::ll -> 
                    f ~classe:"wikimenu_first" a >>= fun first ->
                      mapf ll >>= fun poi ->
                        Lwt.return 
                          {{ [ <ul (classe ++ id) >[ first !{: poi :} ] ] }}
         )
    );


  add_extension_aux ~name:"cond" ~wiki_content:true
    (fun bi args c -> 
       Wikicreole.Block
         (let sp = bi.Wiki_widgets_interface.bi_sp in
          let content = match c with
            | Some c -> c
            | None -> ""
          in
          (let rec eval_cond = function
             | ("error", "autherror") ->
                 Lwt.return
                   (List.exists
                      (fun e -> e = User.BadPassword || e = User.BadUser)
                      (User_services.get_login_error ~sp))
             | ("ingroup", g) ->
                 Lwt.catch
                   (fun () ->
                      User.get_user_by_name g >>= fun group ->
                      User.in_group ~sp ~group ())
                   (function _ -> Lwt.return false)
             | ("http_code", "404") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp =
                     Wiki_widgets_interface.Page_404)
             | ("http_code", "403") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp =
                     Wiki_widgets_interface.Page_403)
             | ("http_code", "40?") ->
                 Lwt.return (Wiki_widgets_interface.page_displayable sp <>
                               Wiki_widgets_interface.Page_displayable)
             | (err, value) when String.length err >= 3 &&
                 String.sub err 0 3 = "not" ->
                 let cond' = (String.sub err 3 (String.length err - 3), value) in
                 eval_cond cond' >>=
                   fun b -> Lwt.return (not b)
                   | _ -> Lwt.return false
           in
           (match args with
              | [c] -> eval_cond c
              | _ -> Lwt.return false)
         >>= function
           | true -> xml_of_wiki wikicreole_parser bi content
           | false -> Lwt.return {{ [] }}
          )
         )
    );

