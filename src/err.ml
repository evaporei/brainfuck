exception Err of string

let setup_printer =
  Printexc.register_printer
    (function
      | Err s -> Some (Printf.sprintf "%s" s)
      | _ -> None
    )
