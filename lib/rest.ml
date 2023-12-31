open Cohttp
open Cohttp_lwt_unix

let (let*) = Lwt.bind

let make_request endpoint =
  let config = Config.read_config () in
  let url = Printf.sprintf "%s%s" config.base_url endpoint in
  let headers = Header.of_list [
    ("Authorization", Printf.sprintf "Bot %s" config.token);
    ("User-Agent", "DiscordBot (https://discord.com/api/v10, 10)")
  ] in
  let* _, body = Client.get ~headers (Uri.of_string url) in
  Lwt.return body

(* TODO: parse body into guild array when the types are done *)
let my_guilds () =
  let* b = make_request "users/@me/guilds" in
  let* s = Cohttp_lwt.Body.to_string b in
  let json = Yojson.Safe.from_string s in
  let r  = match json with
    | `List xs -> List.map Guild.t_of_yojson xs
    | _ -> raise (Failure (Printf.sprintf "my_guilds: List expected. Received:\n%s" s))
  in
  Lwt.return r
