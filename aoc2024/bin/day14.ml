type vec = { x : int; y : int }
type robot = { pos : vec; vel : vec }

let string_of_vec (v : vec) : string = Printf.sprintf "vec: %d, %d" v.x v.y

let string_of_robot (r : robot) : string =
  Printf.sprintf "pos: %d, %d\tvel: %d, %d" r.pos.x r.pos.y r.vel.x r.vel.y

let print_robots (robots : robot list) : unit =
  robots
  |> List.map (fun r -> string_of_robot r)
  |> List.iter (Printf.printf "%s\n")

let scan_robot (line : string) : robot =
  Scanf.sscanf line "p=%d,%d v=%d,%d" (fun px py vx vy ->
      { pos = { x = px; y = py }; vel = { x = vx; y = vy } })

let calc_pos (r : robot) (seconds : int) (bath : vec) : vec =
  let x' = (r.pos.x + (r.vel.x * seconds)) mod bath.x in
  let y' = (r.pos.y + (r.vel.y * seconds)) mod bath.y in
  {
    x = (if x' >= 0 then x' else x' + bath.x);
    y = (if y' >= 0 then y' else y' + bath.y);
  }

let safety_farcor (bath : vec) (p : vec list) : int =
  let rec safety_farcor' p' ul ur dl dr : int =
    match p' with
    | [] -> ul * ur * dl * dr
    | h :: t ->
        if h.x < bath.x / 2 && h.y < bath.y / 2 then
          safety_farcor' t (ul + 1) ur dl dr
        else if h.x > bath.x / 2 && h.y < bath.y / 2 then
          safety_farcor' t ul (ur + 1) dl dr
        else if h.x < bath.x / 2 && h.y > bath.y / 2 then
          safety_farcor' t ul ur (dl + 1) dr
        else if h.x > bath.x / 2 && h.y > bath.y / 2 then
          safety_farcor' t ul ur dl (dr + 1)
        else safety_farcor' t ul ur dl dr
  in
  safety_farcor' p 0 0 0 0

let part1 () =
  let ic = open_in "data/day14.txt" in
  let bath_size = { x = 101; y = 103 } in
  let seconds = 100 in
  In_channel.input_lines ic
  |> List.map (fun x -> scan_robot x)
  |> List.map (fun x -> calc_pos x seconds bath_size)
  |> safety_farcor bath_size |> string_of_int |> print_endline

let is_vec_in_pos (v : vec) (p : vec list) : bool =
  List.for_all (fun v' -> v'.x <> v.x && v'.y <> v.y) p |> not

let render (bath_size : vec) (p : vec list) : unit =
  for i = 0 to bath_size.y - 1 do
    for j = 0 to bath_size.x - 1 do
      if List.exists (fun v' -> v'.y = i && v'.x = j) p then print_string "#"
      else print_string "."
    done;
    print_newline ()
  done;
  print_newline ();
  print_newline ()

module IntSet = Set.Make (struct
  type t = vec

  let compare = Stdlib.compare
end)

let part2 () =
  let ic = open_in "data/day14.txt" in
  let bath_size = { x = 101; y = 103 } in
  let robots = In_channel.input_lines ic |> List.map (fun x -> scan_robot x) in
  for i = 1 to 10000 do
    let n =
      robots
      |> List.map (fun x -> calc_pos x i bath_size)
      |> IntSet.of_list |> IntSet.cardinal
    in
    if n = List.length robots then i |> string_of_int |> print_endline
  done
