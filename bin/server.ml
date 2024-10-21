open Lwt

let n_users = ref 0
let users = Hashtbl.create 10
let listen_address = Unix.inet_addr_loopback
let port = 9090
let backlog = 10

let handle_message msg =
  match msg with
  | "/users" -> string_of_int !n_users
  | "/list_users" -> Hashtbl.fold (fun _ v acc -> acc ^ v ^ "\n") users ""
  | _ -> ""
;;

let rec handle_connection ic oc c_add () =
  Lwt_io.read_line_opt ic
  >>= fun msg ->
  match msg with
  | Some msg ->
    let addr, port = c_add in
    Logs_lwt.info (fun m -> m "%s:%d: %s" addr port msg) |> ignore;
    let reply = handle_message msg in
    if reply <> ""
    then Lwt_io.write_line oc reply >>= handle_connection ic oc c_add
    else handle_connection ic oc c_add ()
  | None -> Logs_lwt.info (fun m -> m "Connection closed") >>= return
;;

let first_time ic oc c_add () =
  let init_message = "Welcome to the chat server!" in
  Lwt_io.write_line oc init_message |> ignore;
  let rec user_name ic () =
    Lwt_io.write_line oc "Please enter your name (Ex. /register [name])" |> ignore;
    Lwt_io.read_line_opt ic
    >>= fun msg ->
    match msg with
    | Some msg ->
      let msg = msg |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
      (match msg with
       | [ "/register"; username ] ->
         (let available = not @@ Hashtbl.mem users c_add in
         match available with
          | true ->
            Hashtbl.add users c_add username;
            handle_connection ic oc c_add ()
          | false -> Lwt_io.write_line oc "Username already taken" >>= user_name ic)
          | _ -> user_name ic ()))
    | None -> user_name ic ()
  in
  user_name ic ()
;;

let accept_connection conn =
  let fd, client_addr = conn in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  match client_addr with
  | Unix.ADDR_INET (addr, port) ->
    n_users := !n_users + 1;
    let ip = Unix.string_of_inet_addr addr in
    Logs_lwt.app (fun m -> m "New Connection from %s:%d" ip port)
    >>= fun () ->
    Lwt.on_failure
      (first_time ic oc (ip, port) ())
      (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e)));
    Lwt.return ()
  | _ -> Logs_lwt.err (fun m -> m "Unknown client address") >>= fun () -> Lwt.return ()
;;

let create_socket () =
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  (bind sock @@ ADDR_INET (listen_address, port) |> fun x -> ignore x);
  listen sock backlog;
  sock
;;

let create_server sock =
  let rec serve () = Lwt_unix.accept sock >>= accept_connection >>= serve in
  serve
;;
