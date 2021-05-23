open Err

let get_file_name =
  match Arg.parse 1 with
    | Some arg -> arg
    | None -> raise (Err "Missing source file name, pass it as first command line argument")

let handle_exc e =
  let err_msg = Printexc.to_string e in
  print_endline @@ "Fatal error: " ^ err_msg;
  exit 1

let () =
  Err.setup_printer;
  try
    let tokens = get_file_name
    |> File.read
    |> Lexer.tokenize in

    let debug_tokens = tokens
    |> List.map Lexer.string_of_token in

    let debug_tokens_output = String.concat ", " debug_tokens in
    print_endline @@ "[" ^ debug_tokens_output ^ "]";

    let commands = match Parser.parse tokens [] false with
    | Ok cmds -> cmds
    | Error e -> raise (Err e)
    in

    let debug_commands = commands
    |> List.map Parser.string_of_command in

    let debug_commands_output = String.concat ", " debug_commands in
    print_endline @@ "[" ^ debug_commands_output ^ "]"
  with e ->
    handle_exc e
