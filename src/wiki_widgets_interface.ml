open Wiki_types
open Tyxml

type box_info = {
  bi_wiki: wiki;
  bi_page: wiki * string;
  bi_sectioning: bool;
  bi_content: Html_types.flow5 Html.elt list Lwt.t
}
