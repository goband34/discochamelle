open Cohttp_lwt_unix

let query_param_list_to_query_string xs = 
  let rec go r = function
  | [] -> r
  | x::xs ->
      match x with
      | `Before s -> go (r ^ Printf.sprintf "before=%s&" s) xs
      | `After s -> go (r ^ Printf.sprintf "after=%s&" s) xs
      | `Limit x -> go (r ^ Printf.sprintf "limit=%d&" x) xs
      | `With_counts b -> go (r ^ Printf.sprintf "with_counts=%b&" b) xs
  in
  match go "" xs with
  | "" -> ""
  | s -> "?" ^ s

let get_all_guilds () =
  let (let*) = Lwt.bind in
  let* _, body = Client.get (Uri.of_string @@ Config.url_for_endpoint "users/@me/guilds") in
  let* body_string = body |> Cohttp_lwt.Body.to_string in
  Lwt.return body_string


