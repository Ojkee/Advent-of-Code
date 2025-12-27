type loc = { y : int; x : int }

let string_of_loc loc = string_of_int loc.y ^ " " ^ string_of_int loc.x
let signals_tbl : (char, loc list) Hashtbl.t = Hashtbl.create 0
let explode s = List.init (String.length s) (String.get s)
let print_loc loc' = Printf.printf "%d %d\n" loc'

let print_board board =
  List.iter (Printf.printf "%s\n")
    (List.map (fun chars -> String.of_seq (List.to_seq chars)) board)

let add_signal name y' x' =
  match Hashtbl.find_opt signals_tbl name with
  | None -> Hashtbl.add signals_tbl name [ { y = y'; x = x' } ]
  | Some locs -> Hashtbl.replace signals_tbl name ({ y = y'; x = x' } :: locs)

let print_loc_list loc_list =
  let loc_to_string loc' = Printf.sprintf "(y: %d, x: %d)" loc'.y loc'.x in
  let loc_strings = List.map loc_to_string loc_list in
  Printf.sprintf "[%s]" (String.concat "; " loc_strings)

let print_signals sig' =
  Hashtbl.iter
    (fun key loc_list ->
      Printf.printf "Key: %c, Values: %s\n" key (print_loc_list loc_list))
    sig'

let rec read_signals board y x =
  match List.nth_opt board y with
  | None -> ()
  | Some row -> (
      match List.nth_opt row x with
      | None -> read_signals board (y + 1) 0
      | Some cell ->
          if cell <> '.' then add_signal cell y x;
          read_signals board y (x + 1))

module LocSet = Set.Make (struct
  type t = loc

  let compare = compare
end)

let get_antinodes lprev lnext =
  let y' = lnext.y - lprev.y in
  let x' = lnext.x - lprev.x in
  [
    { y = lprev.y - y'; x = lprev.x - x' };
    { y = lnext.y + y'; x = lnext.x + x' };
  ]

let flatten src =
  let rec flat s dst = match s with [] -> dst | h :: t -> flat t (dst @ h) in
  flat src []

let rec all_antinodes loc_list dst antinodes_getter =
  match loc_list with
  | [] -> dst
  | [ _ ] -> dst
  | h :: t ->
      dst
      @ flatten (List.map (fun x -> antinodes_getter h x) t)
      @ all_antinodes t [] antinodes_getter

let in_range board loc =
  if loc.x < 0 || loc.y < 0 then false
  else
    match List.nth_opt board loc.y with
    | None -> false
    | Some row -> (
        match List.nth_opt row loc.x with None -> false | Some _ -> true)

let part1 () =
  let ic = open_in "data/day8.txt" in
  let board =
    In_channel.input_all ic |> String.split_on_char '\n'
    |> List.map (fun x -> explode x)
  in
  close_in ic;
  read_signals board 0 0;

  Hashtbl.to_seq signals_tbl
  |> Seq.map (fun x -> snd x)
  |> List.of_seq
  |> List.map (fun x -> all_antinodes x [] get_antinodes)
  |> flatten
  |> List.filter (fun x -> in_range board x)
  |> LocSet.of_list |> LocSet.cardinal |> string_of_int |> print_endline

let get_antinodes_line row_size col_size lprev lnext =
  let dy = lnext.y - lprev.y in
  let dx = lnext.x - lprev.x in
  let rec get_in_line y' x' dy dx dst =
    if x' >= 0 && y' >= 0 && x' < col_size && y' < row_size then
      get_in_line (y' + dy) (x' + dx) dy dx ({ y = y'; x = x' } :: dst)
    else dst
  in
  get_in_line lprev.y lprev.x (-dy) (-dx) []
  @ get_in_line lnext.y lnext.x dy dx []

let part2 () =
  let ic = open_in "data/day8.txt" in
  let col_size = String.length (input_line ic) in
  seek_in ic 0;
  let board_lines = In_channel.input_all ic |> String.split_on_char '\n' in
  let row_size = List.length board_lines - 1 in
  let board = List.map (fun x -> explode x) board_lines in
  close_in ic;

  read_signals board 0 0;
  Hashtbl.to_seq signals_tbl
  |> Seq.map (fun x -> snd x)
  |> List.of_seq
  |> List.map (fun x ->
         all_antinodes x [] (get_antinodes_line row_size col_size))
  |> flatten |> LocSet.of_list |> LocSet.cardinal |> string_of_int
  |> print_endline
