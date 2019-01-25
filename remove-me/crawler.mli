module type S = sig
  type t

  module Entry : sig
    type nonrec t = {
      pred: t option;
      node: t;
      depth: int;
    }
  end

  module Set : Set.S with type elt = Entry.t

  (** [bfs "/" ~add initial ~f] traverses a graph built on-the-fly by [f],
      breadth first. It calls [f] at least once per node.
      [add node] allows declaring successor nodes. *)
  val bfs :
    ?max_depth:int ->
    t list ->
    f:(already: bool ->
       add:(t -> unit) ->
       ?pred:t ->
       t ->
       unit) ->
    Set.t
end


module Make : functor (Ord : Set.OrderedType) -> S with type t = Ord.t
