let map ~f x = match x with None -> None | Some x -> Some (f x)

let bind ~f x = match x with None -> None | Some x -> f x
