  type t =
    | Dev
    | V of string * int list * string option

  let split_char sep p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then
      if cur - beg > 0
      then [String.sub p beg (cur - beg)]
      else []
    else if p.[cur] = sep then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  split 0 0

  let parse s = match String.lowercase_ascii s with
    | "dev" -> Dev
    | s ->
      try
        let s',extra = match split_char '+' s with
          | [] -> assert false
          | [_] -> s, None
          | [x;extra] -> x,Some extra
          | _ -> assert false in
        let l = split_char '.' s' in
        let l = List.map int_of_string l in
        V (s,l,extra)
      with _ -> assert false

  let major = function
    | Dev -> "dev"
    | V (_,x::_,_) -> string_of_int x
    | V (_,[],_) -> assert false

  let compint (a : int) b = compare a b

  let rec compare v v' = match v,v' with
    | Dev, Dev -> 0
    | Dev, _ -> 1
    | _, Dev -> -1
    | V(_,v,_), V(_,v',_) ->
      let rec cmp v v' =
        match v,v' with
        | [x],[y] -> compint x y
        | [],[] -> 0
        | [],y::_ -> compint 0 y
        | x::_,[] -> compint x 0
        | x::xs,y::ys ->
          match compint x y with
          | 0 -> cmp xs ys
          | n -> n
      in cmp v v'

  let match_ pattern v = match pattern,v with
    | "dev", Dev -> true
    | "dev", _ -> false
    | "*" , Dev -> true
    | _   , Dev -> false
    | s , V(_s,l,_e) ->
      let s',_extra = match split_char '+' s with
        | [] -> assert false
        | [_] -> s, None
        | [x;extra] -> x,Some extra
        | _ -> assert false in
      let lpat = split_char '.' s' in
      let rec cmp pat v = match pat,v with
        | [], [] -> true
        | [], _ -> true
        | "*"::_, _ -> true
        | _ , [] -> false
        | x::xs,y::ys when x = string_of_int y -> cmp xs ys
        | _ -> false in
      cmp lpat l

  let to_string = function
    | Dev -> "dev"
    | V(s,_,_) -> s
