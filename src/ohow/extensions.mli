val get_opts :
  ?defaults:string option list ->
  string list (* opts *) ->
  (string * string) list (* args *) ->
  string option list

val register :
  ?defaults:string option list ->
  string (* name *) ->
  string list (* opts *) ->
  (string option ->
  string option list ->
  [< Html_types.span_content_fun > `Span ] Tyxml_html.elt list) ->
  unit
