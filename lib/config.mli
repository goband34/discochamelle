type t = {
  token: string;
  base_url: string
} [@@deriving sexp]

val read_config: unit -> t
val set_config_file: string -> unit
