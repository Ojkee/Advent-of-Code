module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

let to_idx y x col_size = (col_size * y) + x
let explode s = List.init (String.length s) (String.get s)

let print_board board =
  List.iter (Printf.printf "%s\n")
    (List.map (fun chars -> String.of_seq (List.to_seq chars)) board)

let find_in_2d_list grid target =
  let rec find_in_row row col_index =
    match row with
    | [] -> None
    | x :: xs ->
        if x = target then Some col_index else find_in_row xs (col_index + 1)
  in
  let rec find_in_grid grid row_index =
    match grid with
    | [] -> None
    | row :: rows -> (
        match find_in_row row 0 with
        | Some col_index -> Some (row_index, col_index)
        | None -> find_in_grid rows (row_index + 1))
  in
  find_in_grid grid 0

let is_next_obstacle_or_exit board y x dy dx =
  match List.nth_opt board (y + dy) with
  | None -> None
  | Some row -> (
      match List.nth_opt row (x + dx) with
      | None -> None
      | Some cell -> if cell = '#' then Some true else Some false)

let rotated_d dy dx =
  if dy = 1 then (0, -1)
  else if dy = -1 then (0, 1)
  else if dx = -1 then (-1, 0)
  else (1, 0)

let rec simulate_guard board y x dy dx visited col_size =
  let visited = IntMap.add (to_idx y x col_size) true visited in
  match is_next_obstacle_or_exit board y x dy dx with
  | None -> IntMap.cardinal visited
  | Some true ->
      let ndy, ndx = rotated_d dy dx in
      simulate_guard board y x ndy ndx visited col_size
  | Some false -> simulate_guard board (y + dy) (x + dx) dy dx visited col_size

let part1 () =
  let ic = open_in "data/day6.txt" in
  let line_len = String.length (input_line ic) in
  seek_in ic 0;
  let content = In_channel.input_all ic in
  let board =
    List.map (fun x -> explode x) (String.split_on_char '\n' content)
  in
  let visited = IntMap.empty in
  match find_in_2d_list board '^' with
  | Some (row, col) ->
      print_endline
        (string_of_int (simulate_guard board row col (-1) 0 visited line_len))
  | None -> print_endline "Doesn't find starting point"

let part2 () =
  let ic = open_in "data/day6_test.txt" in
  let line_len = String.length (input_line ic) in
  seek_in ic 0;
  let content = In_channel.input_all ic in
  let board =
    List.map (fun x -> explode x) (String.split_on_char '\n' content)
  in
  print_endline (string_of_int line_len);
  print_board board
