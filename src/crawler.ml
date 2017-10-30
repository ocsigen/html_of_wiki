module Make (Ord: Set.OrderedType) = struct

module Entry = struct
  type t = {
    pred: Ord.t option;
    node: Ord.t;
    depth: int;
  }

  let compare {node; _} {node = node'; _} =
    Ord.compare node node'
end

module Set = Set.Make(Entry)

let bfs ?max_depth initial ~f =
  let q = Queue.create () in
  let visited = ref Set.empty in
  Queue.add {Entry.pred = None; node = initial; depth = 0} q;
  while not (Queue.is_empty q) do
    let {Entry.pred; node = cur; depth} as entry = Queue.pop q in
    visited := Set.add entry !visited;
    let add node =
      let nw = {Entry.pred = Some cur; node; depth = depth + 1} in
      match max_depth with
      | Some max when depth >= max -> () (* too deep *)
      | _ when Set.mem nw !visited -> () (* just once *)
      | _ -> Queue.add nw q
    in
    f ~add ?pred cur
  done;
  !visited
end
