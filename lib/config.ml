open Sexplib.Std

type t = {
  token: string;
  base_url: string
} [@@deriving sexp]

let loaded_config: t option ref = ref None

let read_config () =
  match !loaded_config with
  | None -> 
      Sexplib.Sexp.load_sexp "config.scm" |>
      t_of_sexp
  | Some c -> c


