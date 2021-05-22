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
    get_file_name
    |> File.read
    |> print_endline
  with e ->
    handle_exc e
