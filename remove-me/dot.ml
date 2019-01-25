module type Showable = sig
  include Set.OrderedType
  val to_string: t -> string
end


module Make (Node: Showable) = struct
  module C = Crawler.Make(Node)
  type t = Node.t
  module Entry = C.Entry
  module Set = C.Set

  let output = ref stdout
  let set_output ch = output := ch
  let print_endline s =
    output_string !output s;
    output_char !output '\n'

  let bfs ?max_depth initial ~f =
    print_endline "digraph G {";
    let f' ~add ?pred node =
      let rpz x = "\"" ^ String.escaped (Node.to_string x) ^ "\"" in
      (match pred with
      | None -> print_endline (rpz node)
      | Some pr -> print_endline (rpz pr ^ " -> " ^ rpz node));
      f ~add ?pred node
    in
    let set = C.bfs ?max_depth initial ~f:f' in
    print_endline "}";
    set
end
