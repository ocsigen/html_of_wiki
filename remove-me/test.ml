module Link = struct
  type t =
    | A
    | B
  [@@deriving show]

  let compare a b = match a, b with x, y when x = y -> 0 | A, _ -> -1 | _ -> 1

  let to_string = show
end

module C = Crawler.Make (Link)

let () =
  ignore
  @@ C.bfs ~max_depth:5 Link.B ~f:(fun ~add ?pred cur ->
         print_endline @@ Link.show cur;
         add Link.B )
