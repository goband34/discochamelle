open Sexplib.Std

type t = {
  token: string;
  base_url: string
} [@@deriving sexp]

let config_file: string option ref = ref None
let loaded_config: t option ref = ref None

let read_config () =
  match !loaded_config with
  | None -> begin
    match !config_file with
    | None -> raise (Failure "read_config: No config file set")
    | Some file_path ->
      Sexplib.Sexp.load_sexp file_path |>
      t_of_sexp
    end
  | Some c -> c

let set_config_file s =
  config_file := Some s
