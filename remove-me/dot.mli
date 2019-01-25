module type Showable = sig
  include Set.OrderedType

  val to_string : t -> string
end

module Make (Node : Showable) : sig
  include Crawler.S with type t = Node.t

  val set_output : out_channel -> unit
end
