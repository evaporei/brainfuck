let read_file file_name =
  let file_channel = open_in file_name in
    try
      let content = really_input_string file_channel (in_channel_length file_channel) in
      close_in file_channel;
      content
    with e ->
      close_in_noerr file_channel;
      raise e

let () =
  let source_code = read_file "samples/helloworld.bf" in
  print_endline source_code
