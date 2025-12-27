let parse_line line =
  let lhs_rhs = Str.(split_delim (regexp ":") line) in
  let lhs = List.hd lhs_rhs |> int_of_string in
  let rhs =
    List.rev lhs_rhs |> List.hd |> String.trim
    |> Str.(split (regexp " "))
    |> List.filter (fun x -> x <> "")
    |> List.map int_of_string
  in
  (lhs, rhs)

let rec evaluate target src acc =
  match src with
  | [] -> acc = target
  | h :: t -> evaluate target t (acc + h) || evaluate target t (acc * h)

let zero_non_possible cond lhs_rhs =
  let lhs, rhs = lhs_rhs in
  if cond lhs rhs 0 then lhs else 0

let rec sum lst = match lst with [] -> 0 | h :: t -> h + sum t

let part1 () =
  let ic = open_in "data/day7.txt" in
  let content =
    In_channel.input_all ic
    |> Str.(split_delim (regexp_string "\n"))
    |> List.filter (fun x -> not (x = ""))
  in
  close_in ic;

  content
  |> List.map (fun x -> parse_line x |> zero_non_possible evaluate)
  |> sum |> string_of_int |> print_endline

let concat_ints lhs rhs =
  let string_lhs = string_of_int lhs in
  let string_rhs = string_of_int rhs in
  string_lhs ^ string_rhs |> int_of_string

let rec evaluate_concat target src acc =
  match src with
  | [] -> acc = target
  | h :: t ->
      evaluate_concat target t (acc + h)
      || evaluate_concat target t (acc * h)
      || evaluate_concat target t (concat_ints acc h)

let part2 () =
  let ic = open_in "data/day7.txt" in
  let content =
    In_channel.input_all ic
    |> Str.(split_delim (regexp_string "\n"))
    |> List.filter (fun x -> not (x = ""))
  in
  close_in ic;

  content
  |> List.map (fun x -> parse_line x |> zero_non_possible evaluate_concat)
  |> sum |> string_of_int |> print_endline
