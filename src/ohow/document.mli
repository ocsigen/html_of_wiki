type t =
  | Site of string
  | Site_static of string * [ `File | `Folder ]
  | Project of
      { page : project_page
      ; version : Version.t option
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

val parse_page : project:string -> version:Version.t -> string -> t
val parse_page' : project:string -> string -> t
val to_absolute_uri : ?fragment:string -> t -> string
val to_relative_uri : from:t -> ?fragment:string -> t -> string
