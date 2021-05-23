type command =
  | IncrementPointer
  | DecrementPointer
  | Increment
  | Decrement
  | Output
  | Input
  | Loop of command list


        (* match parse ([t]@ts) commands with *)
  (* parse [LoopBegin; Increment; Increment; LoopEnd] [] *)
  (* parse_loop [Increment; Increment; LoopEnd; Comment] [] *)
  (* parse [Increment; Increment; LoopEnd; Comment] [] *)
  (* parse [Increment; LoopEnd; Comment] [Increment] *)
  (* parse [LoopEnd; Comment] [Increment; Increment] *)


  (* parse [LoopBegin; Increment; LoopBegin; Increment; LoopEnd; Increment; LoopEnd; Comment] [] *)
  (* parse_loop [Increment; LoopBegin; Increment; LoopEnd; Increment; LoopEnd; Comment] [] *)
  (* parse [Increment] [] -> Ok [Increment] *)
  (* parse_loop [LoopBegin; Increment; LoopEnd; Increment; LoopEnd; Comment] [Increment] *)

let parse_token t =
  match t with
  | Lexer.IncrementPointer -> Ok IncrementPointer
  | Lexer.DecrementPointer -> Ok DecrementPointer
  | Lexer.Increment -> Ok Increment
  | Lexer.Decrement -> Ok Decrement
  | Lexer.Output -> Ok Output
  | Lexer.Input -> Ok Input
  | _ -> Error "should be done by parent func - unreachable"



  (* o problema parece ser nos tokens passados, está parseando uma vez a mais *)


let rec parse tokens commands in_loop =
  match tokens with
  | [] -> Ok commands (* CENÁRIO: Retorna todos os comandos e finaliza -> pq pode nem ter loop *)
  | t::ts -> match (t, in_loop) with
    | (Lexer.LoopBegin, _) -> (* CENÁRIO: Começar o loop, chamando parse_loop. Deveria finalizar aqui. *)
        (match parse_loop ts [] with
        | Ok (tokens_after, loop_commands) -> parse tokens_after (commands @ [Loop loop_commands]) false
        | Error e -> Error e)
    (* | Lexer.LoopEnd -> (* NÃO PODE TERMINAR AQUI, POIS PRECISAMOS DOS TOKENS SEGUINTES P/ CONTINUAR AFTER LOOP *) *)
    (*     (match parse_loop [t] [] with *)
    (*     (* | Ok (tokens_after, loop_commands) -> parse tokens_after (commands @ [Loop loop_commands]) *) *)
    (*     | Ok (tokens_after, loop_commands) -> Ok commands *)
    (*     | Error e -> Error e) *)
    | (Lexer.LoopEnd, false) -> Error "loop end before begin"
    | (Lexer.LoopEnd, true) -> Ok commands
    | (Lexer.Comment, _) -> parse ts commands in_loop (* CENÁRIO: Ignora comentário e segue *)
    | _ -> match parse_token t with (* CENÁRIO: Adiciona um comando na lista *)
      | Ok c -> parse ts (commands @ [c]) in_loop
      | Error e -> Error e

and parse_loop tokens commands =
  match (tokens, commands) with
  (* | ([Lexer.LoopEnd], []) -> Error "LoopEnd before begin" *)
  | ([], _) -> Error "couldn't find enclosing LoopEnd" (* CENÁRIO: Acabaram os tokens, e não encontrou o LoopEnd*)
  | (t::ts, _) -> match t with
    | Lexer.LoopEnd -> Ok (ts, commands) (* CENÁRIO: Finalizar ao encontrar LoopEnd, e então retornar novos comandos e tokens para o parse continuar no LoopBegin *)
    | _ ->
        match parse ([t] @ ts) commands true with (* CENÁRIO: Fazer o parse de um token -> AQUI ESTÁ O PROBLEMA, QUERO UM TOKEN, MAS SE FOR LOOP BEGIN TEM Q COMEÇAR DE NOVO *)
        (* match parse [t] commands with *)
        (* | Ok (new_cmds) -> parse_loop ts (commands@new_cmds) *)
        | Ok new_cmds -> parse_loop ts new_cmds
        | Error e -> Error e

let rec string_of_command c =
  match c with
  | IncrementPointer -> "IncrementPointer"
  | DecrementPointer -> "DecrementPointer"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Output -> "Output"
  | Input -> "Input"
  | Loop cmds ->
      "Loop [" ^ 
      String.concat ", " (List.map string_of_command cmds)
      ^ "]"
