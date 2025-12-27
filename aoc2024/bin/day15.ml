let explode s = List.init (String.length s) (String.get s)

type vec = { y : int; x : int }

let string_of_vec v = Printf.sprintf "%d, %d" v.y v.x

let get_vecs_of_char (warehouse : char list list) (x_factor : int) (c : char) :
    vec list =
  let rec get_vecs_of_char' (i : int) (j : int) (dst : vec list) =
    match List.nth_opt warehouse i with
    | None -> dst
    | Some row -> (
        match List.nth_opt row j with
        | None -> get_vecs_of_char' (i + 1) 0 dst
        | Some cell ->
            if cell = c then
              get_vecs_of_char' i (j + 1) ({ y = i; x = j * x_factor } :: dst)
            else get_vecs_of_char' i (j + 1) dst)
  in
  get_vecs_of_char' 0 0 []

let char_to_dir (c : char) : vec =
  match c with
  | '^' -> { y = -1; x = 0 }
  | 'v' -> { y = 1; x = 0 }
  | '>' -> { y = 0; x = 1 }
  | '<' -> { y = 0; x = -1 }
  | _ -> { y = 0; x = 0 }

module VecSet = Set.Make (struct
  type t = vec

  let compare = compare
end)

let boxes = ref VecSet.empty
let vec_add (v1 : vec) (v2 : vec) : vec = { y = v1.y + v2.y; x = v1.x + v2.x }

let rec empty_spot_ahead (v : vec) (dir : vec) walls : vec option =
  let next_spot = vec_add v dir in
  if VecSet.mem next_spot walls then None
  else if VecSet.mem next_spot !boxes then empty_spot_ahead next_spot dir walls
  else Some next_spot

let push robot walls (c : char) =
  let dir = char_to_dir c in
  match empty_spot_ahead robot dir walls with
  | None -> robot
  | Some v ->
      boxes := VecSet.add v !boxes;
      boxes := VecSet.remove (vec_add robot dir) !boxes;
      vec_add robot dir

let gps b () : int = VecSet.fold (fun v acc -> acc + (v.y * 100) + v.x) !b 0

let rec perform_moves (robot : vec) (walls : VecSet.t)
    (f_push : vec -> VecSet.t -> char -> vec) b (moves : char list) : int =
  match moves with
  | [] -> gps b ()
  | h :: t ->
      let next_spot = f_push robot walls h in
      perform_moves next_spot walls f_push b t

let part1 () =
  let ic = open_in "data/day15.txt" in
  let content =
    In_channel.input_all ic |> Str.split_delim (Str.regexp "\n\n")
  in
  let warehouse =
    List.nth content 0 |> String.split_on_char '\n'
    |> List.map (fun x -> explode x)
  in
  let walls = get_vecs_of_char warehouse 1 '#' |> VecSet.of_list in
  boxes := get_vecs_of_char warehouse 1 'O' |> VecSet.of_list;
  let robot = get_vecs_of_char warehouse 1 '@' |> List.hd in
  let moves =
    List.nth content 1 |> explode |> List.filter (fun x -> x <> '\n')
  in
  moves
  |> perform_moves robot walls push boxes
  |> string_of_int |> print_endline

let boxes_wide = ref VecSet.empty

let rec empty_spot_ahead_wide (v : vec) (dir : vec) walls : vec option =
  let next_spot = vec_add v dir in
  if VecSet.mem next_spot walls then None
  else if VecSet.mem next_spot !boxes_wide then
    empty_spot_ahead_wide next_spot dir walls
  else Some next_spot

let push_wide robot walls (c : char) =
  let dir = char_to_dir c in
  match empty_spot_ahead_wide robot dir walls with
  | None -> robot
  | Some _ -> failwith "TODO"

(* TEST = 9021 *)
let part2 () =
  let ic = open_in "data/day15_test.txt" in
  let content =
    In_channel.input_all ic |> Str.split_delim (Str.regexp "\n\n")
  in
  let warehouse =
    List.nth content 0 |> String.split_on_char '\n'
    |> List.map (fun x -> explode x)
  in
  let walls = get_vecs_of_char warehouse 2 '#' |> VecSet.of_list in
  boxes_wide := get_vecs_of_char warehouse 2 'O' |> VecSet.of_list;
  let robot = get_vecs_of_char warehouse 2 '@' |> List.hd in
  let moves =
    List.nth content 1 |> explode |> List.filter (fun x -> x <> '\n')
  in
  moves
  |> perform_moves robot walls push_wide boxes_wide
  |> string_of_int |> print_endline
