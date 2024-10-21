(*let () =*)
(*    let () = Logs.set_reporter (Logs.format_reporter ()) in*)
(*    let () = Logs.set_level (Some Logs.Info) in*)
(*    let sock = Server.create_socket () in*)
(*    let serve = Server.create_server sock in*)
(*    Lwt_main.run @@ serve ()*)

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = Server.create_socket () in
  let server = Server.create_server sock in
  Lwt_main.run @@ server ()
;;
