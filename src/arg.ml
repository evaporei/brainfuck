let parse n =
  (Sys.argv
  |> Array.to_list
  |> List.nth_opt)
  n
