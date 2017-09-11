(* This is a dummy implementation of Eliom_lib to provide Option.map *)
module Option = struct
  let map f x = match x with
    | Some x -> Some (f x)
    | None -> None
end
