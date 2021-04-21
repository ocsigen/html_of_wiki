[@@@ocaml.warning "-39"]

type elem =
  [ `Id of string
  | `Container
  ]
[@@deriving json]

type outline_params =
  { elem : elem
  ; restrict : string option
  ; depth : int option
  ; ignore : string list
  ; nav : string
  ; div : bool
  }
[@@deriving json]
