open Tyxml

type href =
  | Absolute of string
  | Document of {document: Document.t; fragment: string option}

type desugar_param = {
  dc_page_wiki : Wiki_types.wiki;
  dc_page_path : string list option;
  mutable dc_warnings : ((int * int) * string) list;
}

(** Type of an action executed on a link in a [Preprocessor.preparse_string]. If
    it returns [Some address] (in LWT), the original address of the link is
    replaced by [address]. The address of the link is kept when the
    [link_action] return [None.]
  *)
type link_action =
    string ->
    string option ->
    Wikicreole.attribs ->
    Wiki_types.wikibox -> string option

module type Preprocessor = sig

  (** [desugar_string dc content] does some possible syntactical desugaring in
      [content]. It should be safe to call this, i.e. there shall be no side
      effects in it. *)
  val desugar_string :
    ?href_action:link_action ->
    ?link_action:link_action ->
    desugar_param -> string -> string

  (** [preparse_string wb content] does possibly some replacements in
      [content] and may have arbitrary side effects in the process
      (e.g. creating wikiboxes etc.). *)
  val preparse_string:
    ?href_action:link_action ->
    ?link_action:link_action ->
    Wiki_types.wikibox -> string -> string

end


(** Module type for representing wikicreole parser whose return type
    id [ret]. *)
module type Parser = sig

  include Preprocessor

  type res

  val from_string:
    sectioning:bool ->
    Wiki_widgets_interface.box_info ->
    string ->
    res Html.elt list list

end

(** A wikicreole_parser is essentially a [Wikicreole.builder] object
    but easily extensible. That is, the fields [plugin] and
    [plugin_action] of [Wikicreole.builder], which are supposed to be
    functions, are here represented as association tables. Thus, it
    becomes easy (and, more importantly, not costly) to add
    extensions. *)
type 'a wikicreole_parser = (module Parser with type res = 'a)

(** Preparser are actually used to rewrite contents of wiki extension
    when storing a wikipage on the database. This is currently used
    only for creating wikibox. *)
type preparser =
    Wiki_types.wikibox ->
    Wikicreole.attribs ->
    string option ->
    string option

type desugarer =
    desugar_param ->
    Wikicreole.attribs ->
    string option ->
    string option

type (+'flow,
      +'flow_without_interactive,
      +'phrasing_without_interactive) plugin_content =
  [ `Flow5_link
      of (href * Wikicreole.attribs * 'flow_without_interactive Html.elt list)
  | `Phrasing_link
      of (href * Wikicreole.attribs * 'phrasing_without_interactive Html.elt list)
  | `Flow5 of 'flow Html.elt list
  | `Phrasing_without_interactive
      of 'phrasing_without_interactive Html.elt list ]

type (+'flow_without_interactive,
      +'phrasing_without_interactive) ni_plugin_content =
  [ `Flow5 of 'flow_without_interactive Html.elt list
  | `Phrasing_without_interactive
      of 'phrasing_without_interactive Html.elt list ]

type (+'flow_without_interactive,
      +'phrasing_without_interactive) link_plugin_content =
  [ `Flow5_link
      of (href * Wikicreole.attribs * 'flow_without_interactive Html.elt list)
  | `Phrasing_link
      of (href * Wikicreole.attribs * 'phrasing_without_interactive Html.elt list) ]


(** Module type for representing extensible wikicreole parser on which
    we can register wiki extension. *)
module rec ExtParser : sig

  module type ExtParser = sig

    include Parser

    type res_without_interactive
    type link_content

    type wikiparser = (res, res_without_interactive, link_content) ExtParser.ext_wikicreole_parser

    val from_string_without_interactive:
      sectioning:bool ->
      Wiki_widgets_interface.box_info ->
      string ->
      res_without_interactive Html.elt list list

    type simple_plugin =
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        string option ->
        (res, res_without_interactive, link_content) plugin_content

    type simple_ni_plugin =
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        string option ->
        (res_without_interactive, link_content) ni_plugin_content

    type 'a wiki_plugin =
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        'a Html.elt list option ->
        (res, res_without_interactive, link_content) plugin_content

    type 'a wiki_ni_plugin =
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        'a Html.elt list option ->
        (res_without_interactive, link_content) ni_plugin_content

    type 'a link_plugin =
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        'a Html.elt list option ->
        (res_without_interactive, link_content) link_plugin_content

    (* Module to encode existential type parameter of the recursive wikiparser.
       Could be replaced by a GADT with Ocaml 3.13. *)
    module type WikiPlugin = sig

      type rec_res
      type rec_res_without_interactive
      type rec_link_content

      val wikiparser:
        (rec_res,
         rec_res_without_interactive,
         rec_link_content) ExtParser.ext_wikicreole_parser
      val update_context:
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        Wiki_widgets_interface.box_info
      val plugin: rec_res wiki_plugin
      val ni_plugin: rec_res_without_interactive wiki_ni_plugin option

    end

    module type LinkPlugin = sig

      type rec_res
      type rec_res_without_interactive
      type rec_link_content

      val wikiparser:
        (rec_res,
         rec_res_without_interactive,
         rec_link_content) ExtParser.ext_wikicreole_parser
      val update_context:
        Wiki_widgets_interface.box_info -> Wikicreole.attribs ->
        Wiki_widgets_interface.box_info
      val plugin: rec_res_without_interactive link_plugin

    end

    module type RawWikiPlugin = sig

      type rec_res
      type rec_res_without_interactive
      type rec_link_content

      val wikiparser:
        (rec_res,
         rec_res_without_interactive,
         rec_link_content) ExtParser.ext_wikicreole_parser
      val plugin: rec_res wikicreole_parser -> simple_plugin
      val ni_plugin:
        (rec_res_without_interactive wikicreole_parser -> simple_ni_plugin) option

    end


    type plugin =
      | SimplePlugin of simple_plugin * simple_ni_plugin option
      | WikiPlugin of (module WikiPlugin)
      | LinkPlugin of (module LinkPlugin)
      | RawWikiPlugin of (module RawWikiPlugin)

    val register_extension: name:string -> ?preparser:preparser -> plugin -> unit

    (**/**)
    val plugin_resolver: Wikicreole.plugin_resolver
    (**/**)

  end

  type ('a, 'b, 'c) ext_wikicreole_parser =
      (module ExtParser
        with type res = 'a
        and type res_without_interactive = 'b
        and type link_content = 'c)

end
