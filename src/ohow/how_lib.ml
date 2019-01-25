(* This is a dummy (and extended) implementation of Eliom_lib... *)

module Option = struct
  let ( >>= ) x f = match x with Some x -> f x | None -> None

  let map f x = x >>= fun x -> Some (f x)

  let default_to def = function Some x -> x | None -> def

  let force = function Some x -> x | None -> raise Not_found
end
