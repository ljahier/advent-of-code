let read_lists filename =
  let ic = open_in filename in
  let rec loop acc_g acc_d =
    try
      let line = input_line ic in
      let line = String.trim line in
      let parts = Str.split (Str.regexp "[ \t]+") line in
      match parts with
      | [g_str; d_str] ->
          let g = int_of_string g_str in
          let d = int_of_string d_str in
          loop (g :: acc_g) (d :: acc_d)
      | _ ->
          failwith "Invalid line format"
    with End_of_file ->
      close_in ic;
      (List.rev acc_g, List.rev acc_d)
  in
  loop [] []
