let distance_total left_list right_list =
  let sorted_left = List.sort compare left_list in
  let sorted_right = List.sort compare right_list in
  if List.length sorted_left != List.length sorted_right then
    failwith "Lists must have the same length"
  else
    List.fold_left2 (fun acc l r -> acc + abs (l - r)) 0 sorted_left sorted_right

let () =
  let (left_list, right_list) = Common.read_lists "input.txt" in
  let dist = distance_total left_list right_list in
  Printf.printf "%d\n" dist
