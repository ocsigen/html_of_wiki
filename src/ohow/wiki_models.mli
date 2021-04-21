(* Ocsimore
 * Copyright (C) 2009
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
(** @author Boris Yakobowski
    @author Vincent Balat *)

type wiki_preprocessor = (module Wiki_syntax_types.Preprocessor)

val identity_preprocessor : wiki_preprocessor

(** See [Wiki_syntax_types.Preprocessor.preparse_string] *)
val preparse_string :
     ?href_action:Wiki_syntax_types.link_action
  -> ?link_action:Wiki_syntax_types.link_action
  -> wiki_preprocessor
  -> Wiki_types.wikibox
  -> string
  -> string

(** See [Wiki_syntax_types.Preprocessor.desugar_string] *)
val desugar_string :
     ?href_action:Wiki_syntax_types.link_action
  -> ?link_action:Wiki_syntax_types.link_action
  -> wiki_preprocessor
  -> Wiki_syntax_types.desugar_param
  -> string
  -> string

type +'res wiki_parser = Wiki_widgets_interface.box_info -> string -> 'res

(* pretty printer *)
