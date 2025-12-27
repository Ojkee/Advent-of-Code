let file_to_lists file_name =
  let ic = open_in file_name in
  let rec read_lines lhs rhs =
    try
      let line = input_line ic in
      let lnum, rnum = Scanf.sscanf line "%d %d" (fun x y -> (x, y)) in
      read_lines (lnum :: lhs) (rnum :: rhs)
    with End_of_file ->
      close_in ic;
      (List.rev lhs, List.rev rhs)
  in
  let lhs, rhs = read_lines [] [] in
  (lhs, rhs)

let part1 () =
  let lhs, rhs = file_to_lists "data/day1.txt" in
  let lhs_sorted = List.sort compare lhs in
  let rhs_sorted = List.sort compare rhs in
  let result =
    List.fold_left2 (fun acc x y -> acc + abs (x - y)) 0 lhs_sorted rhs_sorted
  in
  print_endline (string_of_int result)

let rec occur rhs num total =
  match rhs with
  | [] -> total
  | h :: t -> if h = num then occur t num (total + 1) else occur t num total

let rec my_sum lst total =
  match lst with [] -> total | h :: t -> my_sum t (total + h)

let part2 () =
  let lhs, rhs = file_to_lists "data/day1.txt" in
  let occurs = List.map (fun x -> x * occur rhs x 0) lhs in
  let result = my_sum occurs 0 in
  print_endline (string_of_int result)
