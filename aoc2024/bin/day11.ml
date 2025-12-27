let split_stone (string_stone : string) : int list =
  let str_len = String.length string_stone / 2 in
  let f = String.sub string_stone 0 str_len |> int_of_string in
  let s = String.sub string_stone str_len str_len |> int_of_string in
  [ f; s ]

let blink_stone (stone : int) : int list =
  if stone = 0 then [ 1 ]
  else
    let string_stone = string_of_int stone in
    if String.length string_stone mod 2 = 0 then split_stone string_stone
    else [ stone * 2024 ]

let rec brute_blink times stones =
  if times <= 0 then stones
  else
    brute_blink (times - 1)
      (stones |> List.fold_left (fun acc x -> blink_stone x @ acc) [])

let part1 () =
  let ic = open_in "data/day11.txt" in
  let stones =
    In_channel.input_all ic |> String.split_on_char ' ' |> List.map String.trim
    |> List.map int_of_string
  in
  stones |> brute_blink 25 |> List.length |> string_of_int |> print_endline

module IntMap = Map.Make (struct
  type t = int * int

  let compare = Stdlib.compare
end)

let cache : int IntMap.t ref = ref IntMap.empty

let should_split (stone : int) : bool =
  if String.length (string_of_int stone) mod 2 = 0 then true else false

let split_stone_int (stone : int) : int list =
  stone |> string_of_int |> split_stone

let rec cache_blink (depth : int) (stones : int list) : int =
  if depth = 0 then List.length stones
  else
    match stones with
    | [] -> 0
    | [ 0 ] -> cache_blink (depth - 1) [ 1 ]
    | [ x ] ->
        if should_split x then split_stone_int x |> cache_blink (depth - 1)
        else cache_blink (depth - 1) [ x * 2024 ]
    | h :: t -> (
        match IntMap.find_opt (h, depth) !cache with
        | None ->
            let h_count = cache_blink depth [ h ] in
            cache := IntMap.add (h, depth) h_count !cache;
            h_count + cache_blink depth t
        | Some count -> count + cache_blink depth t)

let part2 () =
  let ic = open_in "data/day11.txt" in
  let stones =
    In_channel.input_all ic |> String.split_on_char ' ' |> List.map String.trim
    |> List.map int_of_string
  in
  stones |> cache_blink 75 |> string_of_int |> print_endline
