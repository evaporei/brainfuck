exception Err of string

let setup_err_printer =
  Printexc.register_printer
    (function
      | Err s -> Some (Printf.sprintf "%s" s)
      | _ -> None
    )

let read_file file_name =
  let file_channel = open_in file_name in
    try
      let content = really_input_string file_channel (in_channel_length file_channel) in
      close_in file_channel;
      content
    with e ->
      close_in_noerr file_channel;
      raise e

let parse_arg n =
  (Sys.argv
  |> Array.to_list
  |> List.nth_opt)
  n

let get_file_name =
  match parse_arg 1 with
    | Some arg -> arg
    | None -> raise (Err "Missing source file name, pass it as first command line argument")

let () =
  setup_err_printer;
  try
    get_file_name
    |> read_file
    |> print_endline
  with e ->
    let err_msg = Printexc.to_string e in
    print_endline @@ "Fatal error: " ^ err_msg;
    exit 1
