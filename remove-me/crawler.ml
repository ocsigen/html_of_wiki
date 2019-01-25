module type S = sig
  type t

  module Entry : sig
    type nonrec t =
      { pred : t option
      ; node : t
      ; depth : int }
  end

  module Set : Set.S with type elt = Entry.t

  val bfs :
       ?max_depth:int
    -> t list
    -> f:(already:bool -> add:(t -> unit) -> ?pred:t -> t -> unit)
    -> Set.t
end

module Make (Ord : Set.OrderedType) = struct
  type t = Ord.t

  module Entry = struct
    type t =
      { pred : Ord.t option
      ; node : Ord.t
      ; depth : int }

    let compare {node; _} {node = node'; _} = Ord.compare node node'
  end

  module Set = Set.Make (Entry)

  let bfs ?max_depth initial ~f =
    let q = Queue.create () in
    let visited = ref Set.empty in
    initial |> List.iter (fun x -> Queue.add {Entry.pred = None; node = x; depth = 0} q);
    while not (Queue.is_empty q) do
      let ({Entry.pred; node = cur; depth} as entry) = Queue.pop q in
      let already = Set.mem entry !visited in
      visited := Set.add entry !visited;
      let add node =
        let nw = {Entry.pred = Some cur; node; depth = depth + 1} in
        match max_depth with
        | Some max when depth >= max -> () (* too deep *)
        | _ when Set.mem nw !visited -> () (* just once *)
        | _ -> Queue.add nw q
      in
      f ~already ~add ?pred cur
    done;
    !visited
end
