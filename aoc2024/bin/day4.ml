let explode s = List.init (String.length s) (String.get s)

let print_board board =
  List.iter (Printf.printf "%s\n")
    (List.map (fun chars -> String.of_seq (List.to_seq chars)) board)

let rec search_forward board y x dy dx text i =
  match List.nth_opt text i with
  | None -> 1
  | Some current_letter -> (
      if x < 0 || y < 0 then 0
      else
        match List.nth_opt board y with
        | None -> 0
        | Some row -> (
            match List.nth_opt row x with
            | None -> 0
            | Some letter ->
                if letter = current_letter then
                  search_forward board (y + dy) (x + dx) dy dx text (i + 1)
                else 0))

let search_all board y x =
  let dirs =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let rec search_around acc = function
    | [] -> acc
    | h :: t ->
        let dy, dx = h in
        search_around
          (acc + search_forward board y x dy dx [ 'X'; 'M'; 'A'; 'S' ] 0)
          t
  in
  search_around 0 dirs

let rec scan_board board y x acc =
  match List.nth_opt board y with
  | None -> acc
  | Some row -> (
      match List.nth_opt row x with
      | None -> scan_board board (y + 1) 0 acc
      | Some _ -> scan_board board y (x + 1) (acc + search_all board y x))

let search_all_x board y x =
  let m1 = search_forward board y x 1 1 [ 'M'; 'A'; 'S' ] 0 in
  let m2 =
    search_forward board y (x + 2) 1 (-1) [ 'M'; 'A'; 'S' ] 0
    + search_forward board (y + 2) x (-1) 1 [ 'M'; 'A'; 'S' ] 0
  in
  let s1 = search_forward board y x 1 1 [ 'S'; 'A'; 'M' ] 0 in
  let s2 =
    search_forward board y (x + 2) 1 (-1) [ 'S'; 'A'; 'M' ] 0
    + search_forward board (y + 2) x (-1) 1 [ 'S'; 'A'; 'M' ] 0
  in
  if (m1 > 0 && m2 > 0) || (s1 > 0 && s2 > 0) then 1 else 0

let rec scan_board_x board y x acc =
  match List.nth_opt board y with
  | None -> acc
  | Some row -> (
      match List.nth_opt row x with
      | None -> scan_board_x board (y + 1) 0 acc
      | Some _ -> scan_board_x board y (x + 1) (acc + search_all_x board y x))

let part1 () =
  let ic = open_in "data/day4.txt" in
  let content = In_channel.input_all ic in
  close_in ic;
  let board =
    List.map (fun x -> explode x) (String.split_on_char '\n' content)
  in
  let result = scan_board board 0 0 0 in
  print_endline (string_of_int result)

let part2 () =
  let ic = open_in "data/day4.txt" in
  let content = In_channel.input_all ic in
  close_in ic;
  let board =
    List.map (fun x -> explode x) (String.split_on_char '\n' content)
  in
  let result = scan_board_x board 0 0 0 in
  print_endline (string_of_int result)
