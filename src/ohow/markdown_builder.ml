module rec MarkdownBuilder : Wikicreole.Builder with type param = unit and type flow = string = struct
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
  let img_elem _ href alt = "![ " ^ alt ^ " ](" ^ href ^ ")"
  let tt_elem _ l = "`" ^ String.concat "" l ^ "`"
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

  let make_href _ url _ = url
  let string_of_href href = href

  let p_elem _ l = (* "debut p" ^ *) String.concat "" l  (*^ " fin p \n" *)
  let pre_elem _ l = "```\n" ^ String.concat "\n" l ^ "\n```\n"
  let h1_elem _ l = "# " ^ String.concat "" l ^ "\n\n"
  let h2_elem _ l = "## " ^ String.concat "" l ^ "\n\n"
  let h3_elem _ l = "### " ^ String.concat "" l ^ "\n\n"
  let h4_elem _ l = "#### " ^ String.concat "" l ^ "\n\n"
  let h5_elem _ l = "##### " ^ String.concat "" l ^ "\n\n"
  let h6_elem _ l = "###### " ^ String.concat "" l ^ "\n\n"
  let section_elem _ l = String.concat "" l

  let ul_elem _ l = String.concat "" (List.map (fun (item, _, _) -> "- " ^ String.concat "" item) l)  ^ "\n"
  let ol_elem _ l = String.concat "" (List.mapi (fun i (item, _, _) -> string_of_int (i + 1) ^ ". " ^ String.concat "" item) l)

  let dl_elem _ l = String.concat "" (List.map (fun (is_title, item, _) -> if is_title then "**" ^ List.hd item ^ "**\n" else ": " ^ String.concat "" item ^ "\n") l) ^ "\n"
  let hr_elem _ = "---\n"

  let table_elem _ l = 
    let rows = List.map (fun (row, _) -> "| " ^ String.concat " | " (List.map (fun (_, _, cell) -> String.concat "" cell) row) ^ " |\n") l in
    String.concat "" rows

  let phrasing s = s
  let flow s = s
  let list s = s

  let error s = "~~" ^ s ^ "~~"

  type plugin_content =
  [ `Flow5_link of (href * Wikicreole.attribs * flow_without_interactive)
  | `Phrasing_link of (href * Wikicreole.attribs * phrasing_without_interactive)
  | `Flow5 of flow
  | `Phrasing_without_interactive of phrasing_without_interactive ]

  let plugin = MarkdownPlugin.plugin

  let plugin_action _ _ _ = fun _ _ _ -> ()
  let link_action _ _ _ _ _ = ()
  let href_action _ _ _ _ _ = ()
end

and MarkdownPlugin : sig
  val plugin : string -> (Wikicreole.plugin_resolver option * (unit -> Wikicreole.attribs -> string option -> MarkdownBuilder.plugin_content)) 
end = struct
  let plugin name =
    match name with
    | "header" ->
        (None, fun _ _  content ->
          let content = match content with Some c -> c | None -> "" in
          let processed_content = Wikicreole.from_string () (module MarkdownBuilder) content in
          `Phrasing_without_interactive (String.concat "" processed_content ^ "\n\n"))
    | "div" ->
        (None, fun _ attribs content ->
          let class_attr = match List.assoc_opt "class" attribs with
            | Some c -> c
            | None -> ""
          in
          let content = match content with Some c -> c | None -> "" in
          let processed_content = Wikicreole.from_string () (module MarkdownBuilder) content in
          `Flow5 ("<div class=\"" ^ class_attr ^ "\">" ^ String.concat "" processed_content ^ "</div>"))
    | "section" ->
        (None, fun _ attribs content ->
          let class_attr = match List.assoc_opt "class" attribs with
            | Some c -> c
            | None -> ""
          in
          let content = match content with Some c -> c | None -> "" in
          let processed_content = Wikicreole.from_string () (module MarkdownBuilder) content in
          `Flow5 ("<section class=\"" ^ class_attr ^ "\">" ^ String.concat "" processed_content ^ "</section>"))
    | "span" ->
        (None, fun _ attribs content ->
          let class_attr = List.assoc "class" attribs in
          let content = match content with Some c -> c | None -> "" in
          let processed_content = Wikicreole.from_string () (module MarkdownBuilder) content in
          `Phrasing_without_interactive ("<span class=\"" ^ class_attr ^ "\">" ^ String.concat "" processed_content ^ "</span>"))
    | "outline" ->
        (None, fun _ _ content ->
          let content = match content with Some c -> c | None -> "" in
          let processed_content = Wikicreole.from_string () (module MarkdownBuilder) content in
          `Flow5 ("<div id=\"overview\" class=\"ocsimore_outline\">" ^ String.concat "" processed_content ^ "</div>"))
    | "a_manual" | "a_api" | "a_api_type" | "a_api_code" | "a_file" ->
        (None, fun _ attribs content ->
          let href = match List.assoc_opt "href" attribs with
            | Some h -> h
            | None -> "#"
          in
          let text = match content with Some c -> c | None -> href in
          `Phrasing_without_interactive ("[" ^ text ^ "](" ^ href ^ ")"))
    | "a_img" ->
        (None, fun _ attribs content ->
          let src = List.assoc "src" attribs in
          let alt = match content with Some c -> c | None -> "" in
          `Phrasing_without_interactive ("![ " ^ alt ^ " ](" ^ src ^ ")"))
    | "code" ->
        (None, fun _ attribs content ->
          let language = match List.assoc_opt "language" attribs with
            | Some lang -> lang
            | None -> "text"
          in
          let class_attr = match List.assoc_opt "class" attribs with
            | Some c -> " ." ^ c
            | None -> ""
          in
          let content = match content with Some c -> c | None -> "" in
          `Phrasing_without_interactive ("```{" ^ "." ^ language ^ class_attr ^ "}\n" ^ content ^ "\n```"))
    (* | default_case ->  *)
        (* print_string ("Handling default case : " ^ default_case ^ " \n");  *)
    | _ ->
        (None, fun _ _ _ -> `Phrasing_without_interactive "")
end

let print_markdown l =
  print_string (String.concat "" l)

let markdown_from_string s =
  print_string (String.concat "" (Wikicreole.from_string  ~sectioning:true () (module MarkdownBuilder) s))
