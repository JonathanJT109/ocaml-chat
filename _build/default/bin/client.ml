(*open Unix*)

(*let uSERVER_IP = inet_addr_loopback*)
(*let uSERVER_PORT : int = 5555*)

let welcome_message =
  "Welcome to the Chat Room!\n\
   Please enter your name to enter the server.\n\
   If you want to send a private message use \"/msg\" followed by the name and the \
   message.\n\n\
  \                    \"/msg John Hello, how are you?\"\n\
  \                    \n\
   If you want to ban someone from texting in the chat room use \"/ban\" followed by the \
   name.\n\
   You will need at least one more person to agree with your action. The user can still \
   view\n\
   public conversations but is not able to type.\n\n\
   If you want to exit use the \"/exit\" command."
;;

let read_message () =
  let msg = ref "" in
  while !msg = "" do
    print_string "-> ";
    let input = read_line () in
    match input with
    | "" -> ()
    | "/exit" ->
      print_endline "Exiting chat...";
      exit 0
    | _ -> msg := input
  done;
  !msg
;;

let read_user_name () =
  print_endline "Enter your name:";
  read_message ()
;;

let send_server msg = print_endline msg

let () =
  print_endline welcome_message;
  let name = read_user_name () in
  send_server name;
  while true do
    let user_message = read_message () in
    send_server user_message
  done;
  ()
;;
