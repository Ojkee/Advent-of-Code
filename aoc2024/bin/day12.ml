module IntLoc = struct
  type t = int * int

  let compare (y0, x0) (y1, x1) =
    match Stdlib.compare y0 y1 with 0 -> Stdlib.compare x0 x1 | c -> c
end

module VisitedLoc = Set.Make (IntLoc)

let visited = ref VisitedLoc.empty
let explode s = List.init (String.length s) (String.get s)

let print_garden (garden : char list list) =
  List.iter
    (fun row ->
      let row_str = String.of_seq (List.to_seq row) in
      Printf.printf "%s\n" row_str)
    garden

let cell_at y x garden =
  match List.nth_opt garden y with
  | None -> None
  | Some row -> (
      match List.nth_opt row x with None -> None | Some cell -> Some cell)

let add_visited loc = visited := VisitedLoc.add loc !visited

let scan_area cell y x garden =
  let rec scan_area' y' x' area perimeters =
    if y' < 0 || x' < 0 then (area, perimeters + 1)
    else
      match cell_at y' x' garden with
      | None -> (area, perimeters + 1)
      | Some current ->
          if cell <> current then (area, perimeters + 1)
          else if not (VisitedLoc.mem (y', x') !visited) then (
            add_visited (y', x');
            let na, np = scan_area' (y' - 1) x' 0 0 in
            let ea, ep = scan_area' y' (x' + 1) 0 0 in
            let sa, sp = scan_area' (y' + 1) x' 0 0 in
            let wa, wp = scan_area' y' (x' - 1) 0 0 in
            (na + ea + sa + wa + area + 1, np + ep + sp + wp + perimeters))
          else (area, perimeters)
  in
  let ar, pe = scan_area' y x 0 0 in
  ar * pe

let scan_garden perimeter_counter garden =
  let rec scan_garden' y x result =
    match List.nth_opt garden y with
    | None -> result
    | Some row -> (
        match List.nth_opt row x with
        | None -> scan_garden' (y + 1) 0 result
        | Some cell ->
            if VisitedLoc.mem (y, x) !visited then scan_garden' y (x + 1) result
            else
              let price = perimeter_counter cell y x garden in
              scan_garden' y (x + 1) (result + price))
  in
  scan_garden' 0 0 0

let part1 () =
  let ic = open_in "data/day12.txt" in
  let garden =
    In_channel.input_all ic |> String.split_on_char '\n' |> List.map String.trim
    |> List.map explode
  in
  garden |> scan_garden scan_area |> string_of_int |> print_endline

let rec flood_fill cell y x garden =
  if VisitedLoc.mem (y, x) !visited then 0
  else if y < 0 || x < 0 then 0
  else
    match List.nth_opt garden y with
    | None -> 0
    | Some row -> (
        match List.nth_opt row x with
        | None -> 0
        | Some c ->
            if c = cell then (
              visited := VisitedLoc.add (y, x) !visited;
              flood_fill cell (y - 1) x garden
              + flood_fill cell (y + 1) x garden
              + flood_fill cell y (x - 1) garden
              + flood_fill cell y (x + 1) garden
              + 1)
            else 0)

let turn_left dy dx =
  if dy = -1 then (0, -1)
  else if dy = 1 then (0, 1)
  else if dx = 1 then (-1, 0)
  else (1, 0)

let can_go_left cell y x ldy ldx garden =
  let dy, dx = turn_left ldy ldx in
  if y + dy < 0 || x + dx < 0 then false
  else
    match List.nth_opt garden (y + dy) with
    | None -> false
    | Some row -> (
        match List.nth_opt row (x + dx) with
        | None -> false
        | Some c -> if c <> cell then false else true)

let turn_right dy dx =
  let dy', dx' = turn_left dy dx in
  (dy' * -1, dx' * -1)

(* TEST = 1206 *)
let part2 () =
  let ic = open_in "data/day12_test.txt" in
  let garden =
    In_channel.input_all ic |> String.split_on_char '\n' |> List.map String.trim
    |> List.map explode
  in
  print_garden garden
