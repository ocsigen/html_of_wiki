module Make : functor (Ord : Set.OrderedType) -> sig
  module Entry : sig
    type t = {
      pred: Ord.t option;
      node: Ord.t;
      depth: int;
    }
  end

  module Set : Set.S with type elt = Entry.t


  (** [bfs "/" ~add initial ~f] traverses a graph built on-the-fly by [f],
      breadth first. [f ~add ?pred node] is called once for each node.
      [add node] allows declaring successor nodes. *)
  val bfs :
    ?max_depth:int ->
    Ord.t ->
    f:(add:(Ord.t -> unit) ->
       ?pred:Ord.t ->
       Ord.t ->
       unit) ->
    Set.t
end
