let explode s = List.init (String.length s) (String.get s)

let init_mem src =
  let rec init_mem' src' dst idx_mem idx_val =
    match src' with
    | [] -> List.rev dst
    | h :: t ->
        if idx_mem mod 2 = 0 then
          init_mem' t
            (List.init h (fun _ -> idx_val) :: dst)
            (idx_mem + 1) (idx_val + 1)
        else
          init_mem' t (List.init h (fun _ -> -1) :: dst) (idx_mem + 1) idx_val
  in
  init_mem' src [] 0 0

let flatten src =
  let rec flat s dst = match s with [] -> dst | h :: t -> flat t (dst @ h) in
  flat src []

let rec get_next_last_rest tail =
  match List.rev tail with
  | [] -> (-1, [])
  | last :: rest ->
      if last <> -1 then (last, List.rev rest)
      else get_next_last_rest (List.rev rest)

let fragment src =
  let rec fragment' src' dst =
    match src' with
    | [] -> List.rev dst
    | h :: t ->
        if h <> -1 then fragment' t (h :: dst)
        else
          let last, rest = get_next_last_rest t in
          fragment' rest (last :: dst)
  in
  fragment' src []

let mul_pos mem =
  let rec mul_pos' mem' idx acc =
    match mem' with
    | [] -> acc
    | h :: t -> mul_pos' t (idx + 1) (acc + (idx * h))
  in
  mul_pos' mem 0 0

let part1 () =
  let ic = open_in "data/day9.txt" in
  let content =
    In_channel.input_all ic |> String.trim |> explode
    |> List.map (fun x -> int_of_char x - 48)
  in
  content |> init_mem |> flatten |> fragment |> mul_pos |> string_of_int
  |> print_endline

let free_spots lst =
  let rec free_spots' lst acc =
    match lst with 
  | [] -> acc
    | h::t -> if h = -1 then free_spots' t (acc + 1) else free_spots' t acc
  in
  free_spots' lst 0

(* 1313165 *)

(* 0...1...2......33333 *)
(* 0...1...233333...... *)
(* 02..1....33333...... *)
(* 021......33333...... *)

let part2 () =
  let ic = open_in "data/day9_test_1.txt" in
  let content =
    In_channel.input_all ic |> String.trim |> explode
    |> List.map (fun x -> int_of_char x - 48)
  in
  
