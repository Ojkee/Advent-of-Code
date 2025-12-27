let explode s = List.init (String.length s) (String.get s)

type vec = { y : int; x : int }

let string_of_vec v = Printf.sprintf "%d, %d" v.y v.x

let print_board board =
  board
  |> List.map (fun c -> String.of_seq (List.to_seq c))
  |> List.iter (Printf.printf "%s\n")

let find_vec (board : char list list) (c : char) : vec =
  let rec find_vec' y' x' =
    match List.nth_opt board y' with
    | None -> { y = 0; x = 0 }
    | Some row -> (
        match List.nth_opt row x' with
        | None -> find_vec' (y' + 1) 0
        | Some cell ->
            if cell = c then { y = y'; x = x' } else find_vec' y' (x' + 1))
  in
  find_vec' 0 0

module VecSet = Set.Make (struct
  type t = vec

  let compare = compare
end)

let visited = VecSet.empty
let find_way s e board : int = failwith ""

(* TEST = 11048 *)
let part1 () =
  let ic = open_in "data/day16_test.txt" in
  let board =
    ic |> In_channel.input_all |> String.split_on_char '\n'
    |> List.map (fun x -> explode x)
  in
  let s = find_vec board 'S' in
  let e = find_vec board 'E' in
  board |> find_way s e |> string_of_int |> print_endline
