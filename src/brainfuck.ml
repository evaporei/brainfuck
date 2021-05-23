open Err

let get_file_name =
  match Arg.parse 1 with
    | Some arg -> arg
    | None -> raise (Err "Missing source file name, pass it as first command line argument")

let handle_exc e =
  let err_msg = Printexc.to_string e in
  print_endline @@ "Fatal error: " ^ err_msg;
  exit 1

type state = {
  tape : int array;
  index : int;
}

let string_of_state s =
  "{\n" ^
  "\ttape: " ^ Array.fold_left (fun acc curr -> acc ^ string_of_int curr ^ ", ") "" s.tape ^ "\n, " ^
  "\tindex: " ^ string_of_int s.index ^ "\n, " ^
  "}"

let increment_pointer st =
  { tape = st.tape; index = st.index + 1 }

let interpret acc_state character =
  match character with
  | '>' -> increment_pointer acc_state
  | _ -> acc_state

let list_of_chars s =
  List.init (String.length s) (String.get s)

let interpreter chars =
  List.fold_left interpret { tape = Array.make 200 0; index = 0 } chars
  (* List.fold_left interpret { tape = Array.make 30000 0; index = 0 } chars *)

let () =
  Err.setup_printer;
  try
    let final_state = get_file_name
    |> File.read
    |> list_of_chars
    |> interpreter
    |> string_of_state
    in

    print_endline final_state
  with e ->
    handle_exc e
