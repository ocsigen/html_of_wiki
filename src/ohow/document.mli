type t =
  | Site of string
  | Project of
      { page : project_page
      ; version : Version.t
      ; project : string
      }
  | Deadlink of exn

and project_page =
  | Static of string * [ `File | `Folder ]
  | Template
  | Page of string
  | Manual of string
  | Api of
      { subproject : string option
      ; file : string
      }

val to_uri : ?fragment:string -> t -> string
