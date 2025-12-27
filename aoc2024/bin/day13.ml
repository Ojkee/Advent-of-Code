type machine = {
  ax : int;
  ay : int;
  bx : int;
  by : int;
  prizex : int;
  prizey : int;
}

let string_of_machine (m : machine) : string =
  Printf.sprintf "A: %d, %d\nB: %d, %d\nPrize: %d, %d\n" m.ax m.ay m.bx m.by
    m.prizex m.prizey

let print_machines (machines : machine list) : unit =
  machines
  |> List.map (fun x -> string_of_machine x)
  |> List.iter (Printf.printf "%s\n")

let scan_machine (line : string) : machine =
  let ax, ay, bx, by, prizex, prizey =
    Scanf.sscanf line
      "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d"
      (fun a b c d e f -> (a, b, c, d, e, f))
  in
  { ax; ay; bx; by; prizex; prizey }

let cost (a : int) (b : int) : int = (3 * a) + b

let mat_of_machine m =
  Lacaml.S.Mat.of_list
    [
      [ float_of_int m.ax; float_of_int m.bx ];
      [ float_of_int m.ay; float_of_int m.by ];
    ]

let vec_of_machine m add10e13 =
  if not add10e13 then
    Lacaml.S.Mat.of_list
      [ [ float_of_int m.prizex ]; [ float_of_int m.prizey ] ]
  else
    Lacaml.S.Mat.of_list
      [
        [ float_of_int (m.prizex + 10000000000000) ];
        [ float_of_int (m.prizey + 10000000000000) ];
      ]

let round (x : float) : float = floor (x +. 0.5)

let round_check x =
  let fx = round x -. x |> Float.abs in
  fx < 0.01

let check_result_list (res : float list) : (int * int) option =
  match res with
  | [] -> None
  | [ x; y ] ->
      if round_check x && round_check y then
        Some (round x |> int_of_float, round y |> int_of_float)
      else None
  | _ :: _ -> None

let lowest_cost (m : machine) add10e13 : int option =
  (* ax=p *)
  (* x=a^-1 * p *)
  let a = mat_of_machine m in
  let p = vec_of_machine m add10e13 in
  (* x is placed in p *)
  Lacaml.S.gesv a p;
  let res = Lacaml.S.Mat.as_vec p |> Lacaml.S.Vec.to_list in
  match check_result_list res with
  | None -> None
  | Some (x, y) -> Some (cost x y)

let part1 () =
  let ic = open_in "data/day13.txt" in
  let machines =
    In_channel.input_all ic
    |> Str.split_delim (Str.regexp "\n\n")
    |> List.map (fun x -> scan_machine x)
  in
  machines
  |> List.map (fun x -> lowest_cost x false)
  |> List.fold_left
       (fun acc x -> match x with None -> acc | Some v -> acc + v)
       0
  |> string_of_int |> print_endline

let cramer (m : machine) (prize_offset : int) : int option =
  let px = m.prizex + prize_offset in
  let py = m.prizey + prize_offset in
  let det_a = (m.ax * m.by) - (m.bx * m.ay) in
  let det_b = (px * m.by) - (m.bx * py) in
  let det_c = (m.ax * py) - (px * m.ay) in
  let x = det_b / det_a in
  let y = det_c / det_a in
  if (x * m.ax) + (y * m.bx) = px && (x * m.ay) + (y * m.by) = py then
    Some (cost x y)
  else None

let part2 () =
  let ic = open_in "data/day13.txt" in
  let machines =
    In_channel.input_all ic
    |> Str.split_delim (Str.regexp "\n\n")
    |> List.map (fun x -> scan_machine x)
  in
  machines
  |> List.map (fun x -> cramer x 10000000000000)
  |> List.fold_left
       (fun acc x -> match x with None -> acc | Some v -> acc + v)
       0
  |> string_of_int |> print_endline
