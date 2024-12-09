let similarity_score left_list right_list =
  let freq = Hashtbl.create 100 in
  List.iter (fun d ->
    Hashtbl.replace freq d ((Hashtbl.find_opt freq d |> Option.value ~default:0) + 1)
  ) right_list;
  List.fold_left (fun acc g ->
    let f = Option.value (Hashtbl.find_opt freq g) ~default:0 in
    acc + (g * f)
  ) 0 left_list

let () =
  let (left_list, right_list) = Common.read_lists "input.txt" in
  let score = similarity_score left_list right_list in
  Printf.printf "%d\n" score
