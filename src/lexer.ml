type token =
  | IncrementPointer
  | DecrementPointer
  | Increment
  | Decrement
  | Output
  | Input
  | LoopBegin
  | LoopEnd
  | Comment

let token_of_char c =
  match c with
  | '>' -> IncrementPointer
  | '<' -> DecrementPointer
  | '+' -> Increment
  | '-' -> Decrement
  | '.' -> Output
  | ',' -> Input
  | '[' -> LoopBegin
  | ']' -> LoopEnd
  | _ -> Comment

let list_of_chars s =
  List.init (String.length s) (String.get s)

let tokenize code =
  code
  |> list_of_chars
  |> List.map token_of_char

let string_of_token t =
  match t with
  | IncrementPointer -> "IncrementPointer"
  | DecrementPointer -> "DecrementPointer"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Output -> "Output"
  | Input -> "Input"
  | LoopBegin -> "LoopBegin"
  | LoopEnd -> "LoopEnd"
  | Comment -> "Comment"
