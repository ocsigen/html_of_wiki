open Import
open Operators
open Wiki_syntax_types

type extensions =
  | Header
  | Div
  | Pre
  | Paragraph
  | Concepts
  | Concept
  | Section
  | Span
  | Outline
  | AManual
  | AApi
  | AApiType
  | AApiCode
  | AFile
  | AImg
  | Code
  | CodeInline
  | Webonly
  | Wip (* work in progress, commentary*)
  | Empty

let extension_table = Hashtbl.create 17

let () =
  Hashtbl.add extension_table "header" Header;
  Hashtbl.add extension_table "div" Div;
  Hashtbl.add extension_table "pre" Pre;
  Hashtbl.add extension_table "paragraph" Paragraph;
  Hashtbl.add extension_table "section" Section;
  Hashtbl.add extension_table "span" Span;
  Hashtbl.add extension_table "outline" Outline;
  Hashtbl.add extension_table "a_manual" AManual;
  Hashtbl.add extension_table "a_api" AApi;
  Hashtbl.add extension_table "a_api_type" AApiType;
  Hashtbl.add extension_table "a_api_code" AApiCode;
  Hashtbl.add extension_table "a_file" AFile;
  Hashtbl.add extension_table "a_img" AImg;
  Hashtbl.add extension_table "code" Code;
  Hashtbl.add extension_table "concepts" Concepts;
  Hashtbl.add extension_table "concept" Concept;
  Hashtbl.add extension_table "code-inline" CodeInline;
  Hashtbl.add extension_table "webonly" Webonly;
  Hashtbl.add extension_table "wip" Wip;
  Hashtbl.add extension_table "" Empty

let rec plugin_res =
  Wikicreole.Resolver
    (fun name ->
      try
        match Hashtbl.find extension_table name with
        | Header
        | Div
        | Section
        | Span
        | Outline
        | Concepts
        | Concept
        | Webonly
        | Pre -> Some plugin_res
        | _ -> None
      with Not_found -> failwith ("plugin name error : " ^ name))

(* retrieve from other files, until `href_of_link_kind` function *)
let a_link_of_uri ?fragment suffix uri contents =
  let uri = suffix <$> (fun s -> Paths.concat_uri_suffix s uri) |? uri in
  let uri = uri ^ (fragment <$> (fun f -> "#" ^ f) |? "") in
  "[" ^ (contents |? uri) ^ "](" ^ uri ^ ")"

let starts_with prefix s =
  let p = String.length prefix in
  String.length s >= p && String.sub s 0 p = prefix

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
            let open Operators in
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
        (let open Operators in
         menu_page |? page)
    | "site" ->
      let file = Global.current_file () in
      let root = Global.root () in
      Absolute
        Paths.(
          rewind root file +/+ !Global.root_to_site
          +/+ String.remove_leading '/' page)
    | p when starts_with "wiki(" p -> wiki_kind p page
    | p when starts_with "wiki" p -> this_wiki_kind p page
    | _ -> failwith @@ "unknown prototype: '" ^ p ^ "'")
  | _ -> failwith @@ "ill formed link: '" ^ addr ^ "'"

let href_of_link_kind bi addr fragment =
  match link_kind bi addr with
  | Absolute a -> (
    match fragment with
    | Some f -> Paths.(a +/+ ("#" ^ f))
    | None -> a)
  | _ -> assert false

let breakline_adder s =
  let len = String.length s in
  let starts_with_newline = len > 0 && s.[0] = '\n' in
  let ends_with_newline = len > 0 && s.[len - 1] = '\n' in
  match (starts_with_newline, ends_with_newline) with
  | true, true -> s
  | true, false -> s ^ "\n"
  | false, true -> "\n" ^ s
  | false, false -> "\n" ^ s ^ "\n"

(* to escape HTML reserved chars in a string *)
let escape_html str =
  let replace_char str c by =
    let rec aux i acc =
      if i >= String.length str
      then acc
      else if str.[i] = c
      then aux (i + 1) (acc ^ by)
      else aux (i + 1) (acc ^ String.make 1 str.[i])
    in
    aux 0 ""
  in
  let steps =
    [ ('<', "\\<")
    ; ('>', "\\>")
    ; ('&', "&amp;")
    ; ('"', "&quot;")
    ; ('\'', "&#39;")
    ; ('`', "\\`")
    ]
  in
  List.fold_left (fun s (c, esc) -> replace_char s c esc) str steps

let get_attributs attrs =
  List.fold_left
    (fun acc (attr, value) -> acc ^ attr ^ "=\"" ^ value ^ "\" ")
    "" attrs

module rec PhrasingParser :
  (Wikicreole.Builder with type param = unit and type flow = string) = struct
  type href = string
  type param = unit
  type phrasing_without_interactive = string
  type phrasing = string
  type flow = string
  type flow_without_interactive = string
  type uo_list = string

  let forbidden = "not allowed"
  let chars s = s
  let strong_elem _ l = "**" ^ String.concat "" l ^ "**"
  let em_elem _ l = "_" ^ String.concat "" l ^ "_"
  let br_elem _ = "  \n"
  let img_elem _ _ _ = forbidden

  let tt_elem _ l =
    "<span class=\"teletype\">" ^ escape_html (String.concat "" l) ^ "</span>"

  let monospace_elem _ l = "`" ^ String.concat "" l ^ "`"
  let underlined_elem _ l = "<u>" ^ String.concat "" l ^ "</u>"
  let linethrough_elem _ l = "~~" ^ String.concat "" l ^ "~~"
  let subscripted_elem _ l = "<sub>" ^ String.concat "" l ^ "</sub>"
  let superscripted_elem _ l = "<sup>" ^ String.concat "" l ^ "</sup>"
  let nbsp = "&nbsp;"
  let endash = "&ndash;"
  let emdash = "&mdash;"
  let a_elem_phrasing _ href l = "[" ^ String.concat "" l ^ "](" ^ href ^ ")"
  let a_elem_flow _ href l = "[" ^ String.concat "" l ^ "](" ^ href ^ ")"
  let make_href = href_of_link_kind
  let string_of_href href = href
  let p_elem _ l = String.concat "" l (* ^ "\n" *)
  let pre_elem _ _ = forbidden
  let h1_elem _ _ = forbidden
  let h2_elem _ _ = forbidden
  let h3_elem _ _ = forbidden
  let h4_elem _ _ = forbidden
  let h5_elem _ _ = forbidden
  let h6_elem _ _ = forbidden
  let section_elem _ _ = forbidden
  let ul_elem _ _ = forbidden
  let ol_elem _ _ = forbidden
  let dl_elem _ _ = forbidden
  let hr_elem _ = forbidden
  let table_elem _ _ = forbidden
  let phrasing s = s
  let flow s = s
  let list s = s
  let error s = "~~" ^ s ^ "~~"

  type plugin_content =
    [ `Flow5_link of href * Wikicreole.attribs * flow_without_interactive
    | `Phrasing_link of href * Wikicreole.attribs * phrasing_without_interactive
    | `Flow5 of flow
    | `Phrasing_without_interactive of phrasing_without_interactive
    ]

  let plugin = MarkdownPlugin.plugin
  let plugin_action _ _ _ _ _ _ = ()
  let link_action _ _ _ _ _ = ()
  let href_action _ _ _ _ _ = ()
end

and MarkdownBuilder :
  (Wikicreole.Builder with type param = unit and type flow = string) = struct
  type href = string
  type param = unit
  type phrasing_without_interactive = string
  type phrasing = string
  type flow = string
  type flow_without_interactive = string
  type uo_list = string

  let chars s = s
  let strong_elem _ l = "**" ^ String.concat "" l ^ "**"
  let em_elem _ l = "_" ^ String.concat "" l ^ "_"
  let br_elem _ = "  \n"

  let img_elem attrs href alt =
    let my_attrs = get_attributs attrs in
    "<img alt=\"" ^ alt ^ "\" src=\"" ^ href ^ "\" " ^ my_attrs ^ ">"

  let tt_elem _ l =
    "<span class=\"teletype\">" ^ escape_html (String.concat "" l) ^ "</span>"

  let monospace_elem _ l = "`" ^ String.concat "" l ^ "`"
  let underlined_elem _ l = "<u>" ^ String.concat "" l ^ "</u>"
  let linethrough_elem _ l = "~~" ^ String.concat "" l ^ "~~"
  let subscripted_elem _ l = "<sub>" ^ String.concat "" l ^ "</sub>"
  let superscripted_elem _ l = "<sup>" ^ String.concat "" l ^ "</sup>"
  let nbsp = "&nbsp;"
  let endash = "&ndash;"
  let emdash = "&mdash;"
  let a_elem_phrasing _ href l = "[" ^ String.concat "" l ^ "](" ^ href ^ ")"
  let a_elem_flow _ href l = "[" ^ String.concat "" l ^ "](" ^ href ^ ")"
  let make_href = href_of_link_kind
  let string_of_href href = href

  (* let p_elem  attrs l =
   *   let my_attrs = " " ^ get_attributs attrs in
   *   "\n<p" ^ my_attrs ^ ">" ^ breakline_adder (String.concat "" l) ^ "</p>\n" *)
  let p_elem attrs l =
    let content = String.concat "" l in
    if attrs = []
    then breakline_adder content ^ "\n"
    else
      let my_attrs = " " ^ get_attributs attrs in
      "\n<p" ^ my_attrs ^ ">" ^ content ^ "</p>\n"

  let pre_elem _ l = "```\n" ^ String.concat "" l ^ "\n```\n"
  let h1_elem _ l = "\n# " ^ String.concat "" l ^ "\n\n"
  let h2_elem _ l = "\n## " ^ String.concat "" l ^ "\n\n"
  let h3_elem _ l = "\n### " ^ String.concat "" l ^ "\n\n"
  let h4_elem _ l = "\n#### " ^ String.concat "" l ^ "\n\n"
  let h5_elem _ l = "\n##### " ^ String.concat "" l ^ "\n\n"
  let h6_elem _ l = "\n###### " ^ String.concat "" l ^ "\n\n"
  let section_elem _ l = String.concat "" l

  let ul_elem _ l =
    String.concat ""
      (List.map (fun (item, _, _) -> "- " ^ String.concat "" item) l)
    ^ "\n"

  let ol_elem _ l =
    String.concat ""
      (List.mapi
         (fun i (item, _, _) ->
           string_of_int (i + 1) ^ ". " ^ String.concat "" item)
         l)

  let dl_elem _ l =
    String.concat ""
      (List.map
         (fun (is_title, item, _) ->
           if is_title
           then "**" ^ List.hd item ^ "**\n"
           else ": " ^ String.concat "" item ^ "\n")
         l)
    ^ "\n"

  let hr_elem _ = "---\n"

  let table_elem _ l =
    let rows =
      List.map
        (fun (row, _) ->
          let cells =
            List.map
              (fun (_, _, cell) -> "<td>" ^ String.concat "" cell ^ "</td>")
              row
          in
          "<tr>" ^ String.concat "" cells ^ "</tr>\n")
        l
    in
    "<table>\n<tbody>\n" ^ String.concat "" rows ^ "</tbody>\n</table>\n"

  let phrasing s = s
  let flow s = s
  let list s = s
  let error s = "~~" ^ s ^ "~~"

  type plugin_content =
    [ `Flow5_link of href * Wikicreole.attribs * flow_without_interactive
    | `Phrasing_link of href * Wikicreole.attribs * phrasing_without_interactive
    | `Flow5 of flow
    | `Phrasing_without_interactive of phrasing_without_interactive
    ]

  let plugin = MarkdownPlugin.plugin
  let plugin_action _ _ _ _ _ _ = ()
  let link_action _ _ _ _ _ = ()
  let href_action _ _ _ _ _ = ()
end

and MarkdownPlugin : sig
  val plugin :
       string
    -> Wikicreole.plugin_resolver option
       * (   unit
          -> Wikicreole.attribs
          -> string option
          -> MarkdownBuilder.plugin_content)
end = struct
  let get_function_of_extension (ext : extensions) =
    match ext with
    | Header ->
      ( Some plugin_res
      , fun _ _ content ->
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Phrasing_without_interactive
            ("<header>\n"
            ^ String.concat "" processed_content
            ^ "\n</header> \n") )
    | Pre ->
      ( Some plugin_res
      , fun _ attribs content ->
          let my_attrs = get_attributs attribs in
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module PhrasingParser) content
          in
          `Flow5
            ("<pre " ^ my_attrs ^ ">\n"
            ^ String.concat "" processed_content
            ^ "\n</pre>") )
    | Div ->
      ( Some plugin_res
      , fun _ attribs content ->
          let my_attrs = get_attributs attribs in
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Flow5
            ("<div " ^ my_attrs ^ ">\n"
            ^ String.concat "" processed_content
            ^ "\n</div>") )
    | Paragraph ->
      ( None
      , fun _ _ content ->
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          `Phrasing_without_interactive
            ("<div class=\"paragraph\">\n <p>" ^ content ^ "</p>\n</div>") )
    | Concepts ->
      ( Some plugin_res
      , fun _ _ content ->
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Flow5
            ("<aside class=\"concepts\">\n\
              <header><h5>Concepts</h5></header>\n\
              <p>"
            ^ String.concat "" processed_content
            ^ "</p>\n</aside>") )
    | Concept ->
      ( Some plugin_res
      , fun _ attribs content ->
          let title = List.assoc_opt "title" attribs in
          let title_html =
            match title with
            | Some t ->
              "<header><h5><span class=\"concept_prefix\">Concept: </span>" ^ t
              ^ "</h5></header>\n"
            | None -> ""
          in
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Flow5
            ("<aside class=\"concept\">\n" ^ title_html ^ "<p>"
            ^ String.concat "" processed_content
            ^ "</p>\n</aside>") )
    | Section ->
      ( Some plugin_res
      , fun _ attribs content ->
          let my_attrs = get_attributs attribs in
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Flow5
            ("<section " ^ my_attrs ^ " >"
            ^ String.concat "" processed_content
            ^ "</section>") )
    | Span ->
      ( Some plugin_res
      , fun _ attribs content ->
          let my_attrs = get_attributs attribs in
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module PhrasingParser) content
          in
          `Phrasing_without_interactive
            ("<span " ^ my_attrs ^ " >"
            ^ String.concat "" processed_content
            ^ "</span>") )
    | Outline ->
      ( Some plugin_res
      , fun _ _ content ->
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          let processed_content =
            Wikicreole.from_string () (module MarkdownBuilder) content
          in
          `Flow5
            ("<div id=\"overview\" class=\"ocsimore_outline\">"
            ^ String.concat "" processed_content
            ^ "</div>") )
    (* retrieve from another file until AApiCode, I modified it a little bit *)
    | AManual ->
      ( None
      , fun _ attribs content ->
          let project = List.assoc_opt "project" attribs in
          let chapter = List.assoc_opt "chapter" attribs in
          let fragment = List.assoc_opt "fragment" attribs in
          let version = "latest" in
          let file = Global.current_file () in
          let root, manual = Global.(root (), the_manual ()) in
          let uri =
            match (project, chapter) with
            | Some p, Some c ->
              Paths.(
                rewind root file
                (* inside this version dir *)
                +/+ !Global.root_to_site
                (* inside project dir *)
                +/+ p
                +/+ version +/+ manual +/+ c)
            | Some p, None ->
              Paths.(
                rewind root file +/+ !Global.root_to_site +/+ p +/+ "index.html")
            | None, Some c -> Paths.(rewind root file +/+ manual +/+ c)
            | None, None ->
              failwith "a_manual: no project nor chapter arg found"
          in
          let link =
            match fragment with
            | Some fragment -> a_link_of_uri ~fragment
            | None -> a_link_of_uri ?fragment:None
          in
          `Phrasing_without_interactive
            (link (Some (Global.suffix ())) uri content) )
    | AApi | AApiType ->
      ( None
      , fun _ attribs content ->
          let project = List.assoc_opt "project" attribs in
          let subproject = List.assoc_opt "subproject" attribs in
          let text = List.assoc_opt "text" attribs in
          let version = "latest" in
          let kind_opt = [] in
          let kind =
            match kind_opt with
            | [ Some "odoc" ] -> `Odoc
            | [ Some "ocamldoc" ] -> `Ocamldoc
            | [ Some _ ] -> `Ocamldoc
            | [ None ] | [] -> (
              let target_project =
                match project with
                | None -> (Global.options ()).project
                | Some p -> Some p
              in
              match target_project with
              | Some "js_of_ocaml" -> (
                match version with
                | "latest" | "dev" -> `Odoc
                | v ->
                  let v = Version.parse v in
                  if Version.compare v (Version.parse "3.5.0") < 0
                  then `Ocamldoc
                  else `Odoc)
              | Some _ -> `Ocamldoc
              | None -> `Ocamldoc)
            | _ :: _ :: _ -> assert false
          in
          let prefix =
            None
            (* I remove that because we don't have it as a parameter *)
            (* match kind with
             * | `Odoc -> None
             * | `Ocamldoc -> prefix *)
          in
          let file = Global.current_file () in
          let root, api = Global.(root (), the_api ()) in
          let id = Api.parse_contents (content <$> String.trim) in
          let dsp = (Global.options ()).default_subproject in
          let base =
            match (project, subproject, dsp, kind) with
            | Some p, Some s, _, _ ->
              Paths.(
                rewind root file +/+ !Global.root_to_site +/+ p +/+ version
                +/+ api +/+ s)
            | Some p, None, _, `Ocamldoc ->
              Paths.(
                rewind root file +/+ !Global.root_to_site +/+ p +/+ version
                +/+ api)
            | Some p, None, _, `Odoc ->
              Paths.(
                rewind root file +/+ !Global.root_to_site +/+ p +/+ version
                +/+ api +/+ p)
            | None, Some s, _, _ | None, None, Some s, _ ->
              Paths.(rewind root file +/+ api +/+ s)
            | None, None, None, _ -> Paths.rewind root file +/+ api
          in
          let path_of_id =
            match (kind, prefix) with
            | `Ocamldoc, ((None | Some _) as prefix) ->
              Api.Ocamldoc.path_of_id ?prefix id
            | `Odoc, None -> Api.Odoc.path_of_id id
            | `Odoc, Some _ -> assert false
          in
          let uri = Filename.concat base @@ path_of_id in
          let fragment =
            match kind with
            | `Ocamldoc -> Api.Ocamldoc.fragment_of_id id
            | `Odoc -> Api.Odoc.fragment_of_id id
          in
          let body = text |? Api.string_of_id ~spacer:"." id in

          let link =
            match fragment with
            | Some fragment -> a_link_of_uri ~fragment
            | None -> a_link_of_uri ?fragment:None
          in
          `Phrasing_without_interactive
            (link (Some (Global.suffix ())) uri (Some body)) )
    | AFile ->
      ( None
      , fun _ attribs content ->
          let href =
            match List.assoc_opt "src" attribs with
            | Some h -> h
            | None -> "#"
          in
          let text =
            match content with
            | Some c -> c
            | None -> href
          in
          `Phrasing_without_interactive ("[" ^ text ^ "](files/" ^ href ^ ")")
      )
    | AApiCode ->
      ( None
      , fun _ attribs content ->
          let href =
            match List.assoc_opt "src" attribs with
            | Some h -> h
            | None -> "#"
          in
          let text =
            match content with
            | Some c -> c
            | None -> href
          in
          `Phrasing_without_interactive ("[" ^ text ^ "](" ^ href ^ ")") )
    | AImg ->
      ( None
      , fun _ attribs content ->
          let my_attrs = get_attributs attribs in
          let alt =
            match content with
            | Some c -> c
            | None -> ""
          in
          `Phrasing_without_interactive
            ("<img alt=\"" ^ alt ^ "\" " ^ my_attrs ^ ">") )
    | Code ->
      ( None
      , fun _ attribs content ->
          let my_classes =
            List.fold_left (fun acc (_, c) -> acc ^ "." ^ c ^ " ") "" attribs
          in
          let content =
            match content with
            | Some c -> breakline_adder c
            | None -> ""
          in
          `Phrasing_without_interactive
            ("```{" ^ my_classes ^ "}" ^ content ^ "\n```") )
    | CodeInline ->
      ( None
      , fun _ _ content ->
          let content =
            match content with
            | Some c -> c
            | None -> ""
          in
          `Phrasing_without_interactive ("`" ^ content ^ "`") )
    | Webonly ->
      ( Some plugin_res
      , fun _ _ content ->
          match content with
          | None -> `Phrasing_without_interactive ""
          | Some content ->
            let processed_content =
              Wikicreole.from_string () (module MarkdownBuilder) content
            in
            `Phrasing_without_interactive (String.concat "" processed_content)
      )
    | Wip -> (None, fun _ _ _ -> `Phrasing_without_interactive "")
    | Empty -> (None, fun _ _ _ -> `Phrasing_without_interactive "")

  let plugin name =
    try
      let ext = Hashtbl.find extension_table name in
      get_function_of_extension ext
    with Not_found -> failwith (name ^ "plugin name error")
end

let write_markdown oc s =
  output_string oc
    (String.concat ""
       (Wikicreole.from_string ~sectioning:true () (module MarkdownBuilder) s));
  flush oc;
  close_out oc (* Ensure oc is closed after markdown conversion *)