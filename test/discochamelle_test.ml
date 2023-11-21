open Lwt
open Discochamelle

let test = 
  Rest.my_guilds () >>= fun s ->
    Lwt_io.printl s

let () =
  Lwt_main.run test
