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
   Wiki AST to OcamlDuce
   @author Vincent Balat
*)

open Wiki_sql.Types


(** The type for a function acting as a syntax extension *)
type syntax_extension =
    (Wiki_widgets_interface.box_info,
     Xhtmltypes_duce.flows Lwt.t,
     Eliom_duce.Blocks.a_content_elt_list Lwt.t)
   Wikicreole.plugin


(** The abstract type of the objects able to parse wiki creole syntax,
    possibly with extensions. Those objects are passed as arguments
    to all displaing functions *)
type wiki_parser


(** Add a syntax extension to an existing syntax parser
    XXX Document better *)
val add_extension :
  wp:wiki_parser ->
  name:string ->
  ?wiki_content:bool ->
  syntax_extension ->
  unit


(** The default syntax parser. It parses wiki creole syntax, as well
    as div, span, wikiname, raw, content, menu and cond tags.
    Currently modified in Wiki_widgets and User_widgets *)
val default_parser : wiki_parser

(** Return a copy of a parser. The calls to [add_extension] on one of the
    copy will not be visible on the other *)
val copy_parser : wiki_parser -> wiki_parser



(** Functions called to transform some wikicreole text *)
val add_preparser_extension :
  wp:wiki_parser ->
  name:string ->
  ( Eliom_sessions.server_params * Wiki_sql.Types.wikibox,
    string option Lwt.t)
  Wikicreole.plugin_args ->
  unit

val preparse_extension :
  wiki_parser ->
  (Eliom_sessions.server_params * Wiki_sql.Types.wikibox) ->
  string -> string Lwt.t



(** **)

(** Functions displaying wikicreole code *)


(** Returns the XHTML corresponding to a wiki page *)
val xml_of_wiki :
  wiki_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  Xhtmltypes_duce.flows Lwt.t

(** returns only the content of the first paragraph of a wiki text. *)
val inline_of_wiki :
  wiki_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  Xhtmltypes_duce.inlines Lwt.t

(** returns only the content of the first paragraph of a wiki text,
    after having removed links. *)
val a_content_of_wiki :
  wiki_parser ->
  Wiki_widgets_interface.box_info ->
  string ->
  {{ [ Xhtmltypes_duce.a_content* ] }} Lwt.t

(** Returns the wiki syntax for an extension box
    from its name, arguments and content.
*)
val string_of_extension : 
  string -> (string * string) list -> string option -> string

(** parses common attributes ([class], [id]) *)
val parse_common_attribs : (string * string) list -> Xhtmltypes_duce.coreattrs

(** returns true if the string is an absolute URL (http://...) *)
val is_absolute_link : string -> bool


