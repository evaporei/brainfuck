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
  in_loop : bool;
}

let string_of_state s =
  "{\n" ^
  "\ttape: " ^ Array.fold_left (fun acc curr -> acc ^ string_of_int curr ^ ", ") "" s.tape ^ "\n, " ^
  "\tindex: " ^ string_of_int s.index ^ "\n, " ^
  "}"

let increment_pointer st =
  { tape = st.tape; index = st.index + 1; in_loop = st.in_loop }

let decrement_pointer st =
  { tape = st.tape; index = st.index - 1; in_loop = st.in_loop }

let increment_cell st =
  let new_val = st.tape.(st.index) + 1 in
  Array.set st.tape st.index new_val;
  st

let decrement_cell st =
  let new_val = st.tape.(st.index) - 1 in
  Array.set st.tape st.index new_val;
  st

let output st =
  let character = st.tape.(st.index) in
  print_char @@ Char.chr character;
  st

let input st =
  let user_input = Scanf.scanf "%c" Fun.id in
  let char_code = Char.code user_input in
  Array.set st.tape st.index char_code;
  st

let interpret acc_state character =
  match (character, acc_state.in_loop) with
  | ('>', _) -> increment_pointer acc_state
  | ('<', _) -> decrement_pointer acc_state
  | ('+', _) -> increment_cell acc_state
  | ('-', _) -> decrement_cell acc_state
  | ('.', _) -> output acc_state
  | (',', _) -> input acc_state
  | _ -> acc_state

let list_of_chars s =
  List.init (String.length s) (String.get s)

let interpreter chars =
  List.fold_left interpret { tape = Array.make 200 0; index = 0; in_loop = false } chars
  (* List.fold_left interpret { tape = Array.make 30000 0; index = 0; in_loop = false } chars *)

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
