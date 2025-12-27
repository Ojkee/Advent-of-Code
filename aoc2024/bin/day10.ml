let explode s = List.init (String.length s) (String.get s)

type loc = { y : int; x : int }

let trailheads_tbl : (loc, loc list) Hashtbl.t = Hashtbl.create 0

module LocSet = Set.Make (struct
  type t = loc

  let compare = compare
end)

let rec explore_trailhead grid y' x' current dst =
  if y' < 0 || x' < 0 then dst
  else
    match List.nth_opt grid y' with
    | None -> dst
    | Some row -> (
        match List.nth_opt row x' with
        | None -> dst
        | Some cell ->
            if cell = current && current = 9 then { y = y'; x = x' } :: dst
            else if cell = current then
              explore_trailhead grid y' (x' - 1) (current + 1) dst
              @ explore_trailhead grid y' (x' + 1) (current + 1) dst
              @ explore_trailhead grid (y' - 1) x' (current + 1) dst
              @ explore_trailhead grid (y' + 1) x' (current + 1) dst
            else [])

let scan_grid trailheads_tbl grid =
  let rec scan_grid' y' x' =
    match List.nth_opt grid y' with
    | None -> trailheads_tbl
    | Some row -> (
        match List.nth_opt row x' with
        | None -> scan_grid' (y' + 1) 0
        | Some cell ->
            if cell = 0 then (
              let finished = explore_trailhead grid y' x' 0 [] in
              Hashtbl.add trailheads_tbl { y = y'; x = x' } finished;
              scan_grid' y' (x' + 1))
            else scan_grid' y' (x' + 1))
  in
  scan_grid' 0 0

let print_grid grid =
  let print_inner_list inner_grid =
    let inner_str = String.concat " " (List.map string_of_int inner_grid) in
    Printf.printf "%s\n" inner_str
  in
  List.iter print_inner_list grid

let trailheads_acc trailheads_tbl =
  Hashtbl.fold
    (fun _ trailheads acc ->
      acc + (trailheads |> LocSet.of_list |> LocSet.cardinal))
    trailheads_tbl 0

let part1 () =
  let ic = open_in "data/day10.txt" in
  let grid =
    In_channel.input_lines ic
    |> List.map (fun x -> String.trim x)
    |> List.map (fun x ->
           explode x |> List.map int_of_char |> List.map (fun y -> y - 48))
  in
  grid |> scan_grid trailheads_tbl |> trailheads_acc |> string_of_int
  |> print_endline

let trailheads_acc_rating trailheads_tbl =
  Hashtbl.fold
    (fun _ trailheads acc -> acc + (trailheads |> List.length))
    trailheads_tbl 0

let part2 () =
  let ic = open_in "data/day10.txt" in
  let grid =
    In_channel.input_lines ic
    |> List.map (fun x -> String.trim x)
    |> List.map (fun x ->
           explode x |> List.map int_of_char |> List.map (fun y -> y - 48))
  in
  grid |> scan_grid trailheads_tbl |> trailheads_acc_rating |> string_of_int
  |> print_endline
