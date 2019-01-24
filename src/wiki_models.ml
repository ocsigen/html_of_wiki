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
(**
   @author Vincent Balat
   @author Boris Yakobowski
*)

type wiki_preprocessor = (module Wiki_syntax_types.Preprocessor)

let identity_preprocessor =
  let module Identity_preprocessor = struct
    let preparse_string ?href_action:_ ?link_action:_ _ s = s
    let desugar_string ?href_action:_ ?link_action:_ _ s = s
  end in
  (module Identity_preprocessor : Wiki_syntax_types.Preprocessor)

let preparse_string ?href_action ?link_action wpp p c =
  let module Preprocessor = (val wpp : Wiki_syntax_types.Preprocessor) in
  Preprocessor.preparse_string ?href_action ?link_action p c

let desugar_string ?href_action ?link_action wpp p c =
  let module Preprocessor = (val wpp : Wiki_syntax_types.Preprocessor) in
  Preprocessor.desugar_string ?href_action ?link_action p c

type +'res wiki_parser =
  Wiki_widgets_interface.box_info -> string -> 'res (* pretty printer *)
