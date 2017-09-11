open Wiki_types

type box_info = {
  bi_wiki: wiki;
  bi_page: wiki * string;
  bi_sectioning: bool;
}
