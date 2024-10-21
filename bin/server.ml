open Lwt
open Spectrum

(*TODO: Add and Assign color to users*)
(*TODO: How to allow other devices to connect to the server?*)
(*TODO: Add server commands*)

let users
  : (string, string * int * Lwt_io.input_channel * Lwt_io.output_channel) Hashtbl.t
  =
  Hashtbl.create 10
;;

let listen_address = Unix.inet_addr_loopback
let n_users = ref 0
let port = 9090
let backlog = 10
let reset_ppf = prepare_ppf Format.std_formatter
let server_msg msg = Simple.sprintf "@{<#ff8700>@{<bold>SERVER:@} %s@}" msg

let send_to sender receiver msg () =
  let msg = "[" ^ sender ^ "]: " ^ msg in
  let found = Hashtbl.mem users receiver in
  match found with
  | true ->
    let _, _, _, oc = Hashtbl.find users receiver in
    Lwt_io.write_line oc msg |> ignore;
    Lwt_io.flush oc |> ignore
  | false ->
    let _, _, _, soc = Hashtbl.find users sender in
    Lwt_io.write_line soc ("User not found" |> server_msg) |> ignore
;;

let send_all sender msg () =
  let msg = "[" ^ sender ^ "]: " ^ msg in
  Hashtbl.iter
    (fun name (_, _, _, oc) ->
      if name <> sender
      then (
        Lwt_io.write_line oc msg |> ignore;
        Lwt_io.flush oc |> ignore))
    users
;;

let handle_message sender msg () =
  let args = msg |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
  if List.length args = 0
  then ""
  else (
    let command = List.hd args in
    match command with
    | "/nusers" -> string_of_int !n_users
    | "/users" -> Hashtbl.fold (fun k _ acc -> acc ^ k ^ "\n") users "Users:\n"
    | "/msg" ->
      (match args with
       | [ "/msg"; username; msg ] ->
         send_to sender username msg ();
         ""
       | _ -> "Wrong number of arguments")
    | _ ->
      send_all sender msg ();
      "")
;;

let rec handle_connection ic oc username () =
  Lwt_io.read_line_opt ic
  >>= fun msg ->
  match msg with
  | Some msg ->
    let received_msg =
      Simple.sprintf "@{<blue>@{<bold>[%s]:@} %s@}" username (msg |> String.trim)
    in
    Logs_lwt.info (fun m -> m "%s" received_msg) |> ignore;
    let reply = handle_message username msg () in
    if reply <> ""
    then Lwt_io.write_line oc reply >>= handle_connection ic oc username
    else handle_connection ic oc username ()
  | None ->
    Logs_lwt.info (fun m ->
      n_users := !n_users - 1;
      m "CONNECTION CLOSED")
    >>= return
;;

let first_time ic oc c_add () =
  let init_message = "Welcome to the chat server!" in
  Lwt_io.write_line oc (init_message |> server_msg) |> ignore;
  let rec get_username ic () : string Lwt.t =
    Lwt_io.write_line oc ("Please enter your name (Ex. /register [name])" |> server_msg)
    |> ignore;
    Lwt_io.read_line_opt ic
    >>= fun msg ->
    match msg with
    | Some msg ->
      let msg = msg |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
      (match msg with
       | [ "/register"; name ] ->
         let available = not @@ Hashtbl.mem users name in
         (match available with
          | true ->
            let addr, port = c_add in
            Hashtbl.add users name (addr, port, ic, oc);
            Lwt_io.write_line oc ("Successfully Registered" |> server_msg)
            >>= fun () -> Lwt.return name
          | false ->
            Lwt_io.write_line oc ("Username already taken" |> server_msg)
            >>= get_username ic)
       | _ ->
         Lwt_io.write_line oc ("Wrong number of arguments" |> server_msg)
         >>= get_username ic)
    | None -> get_username ic ()
  in
  get_username ic () >>= fun username -> handle_connection ic oc username ()
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
