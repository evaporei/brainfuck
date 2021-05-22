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

    let debug_list = tokens
    |> List.map Lexer.string_of_token in

    let debug_output = String.concat ", " debug_list in
    print_endline @@ "[" ^ debug_output ^ "]"
  with e ->
    handle_exc e
