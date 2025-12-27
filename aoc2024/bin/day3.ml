let parse_match_mul matched =
  let a, b = Scanf.sscanf matched "mul(%d,%d)" (fun x y -> (x, y)) in
  if a > 999 || b > 999 then 0 else a * b

let rec find_matched re line acc pos =
  try
    let _ = Str.search_forward re line pos in
    let matched = Str.matched_string line in
    let current_match_val = parse_match_mul matched in
    find_matched re line (acc + current_match_val) (Str.match_end ())
  with Not_found -> acc

let part1 () =
  let ic = open_in "data/day3.txt" in
  let re_mul = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let content = In_channel.input_all ic in
  let result = find_matched re_mul content 0 0 in
  close_in ic;
  print_endline (string_of_int result)

let can_find_in_substr re content pos =
  try
    let _ = Str.search_forward re content pos in
    true
  with Not_found -> false

let rec find_matched_toggle content enabled pos acc =
  let re_do = Str.regexp "do()" in
  let re_dont = Str.regexp "don't()" in
  let re_mul = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  try
    if not enabled then
      let _ = Str.search_forward re_do content pos in
      find_matched_toggle content true (Str.match_end ()) acc
    else if not (can_find_in_substr re_mul content pos) then acc
    else if not (can_find_in_substr re_dont content pos) then
      let _ = Str.search_forward re_mul content pos in
      let mul_end = Str.match_end () in
      let matched_mul = Str.matched_string content in
      let next_val = parse_match_mul matched_mul in
      find_matched_toggle content true mul_end (acc + next_val)
    else
      let dont_idx = Str.search_forward re_dont content pos in
      let dont_end = Str.match_end () in
      let mul_idx = Str.search_forward re_mul content pos in
      let mul_end = Str.match_end () in
      if dont_idx < mul_idx then find_matched_toggle content false dont_end acc
      else
        let matched_mul = Str.matched_string content in
        let next_val = parse_match_mul matched_mul in
        find_matched_toggle content true mul_end (acc + next_val)
  with Not_found -> acc

let part2 () =
  let ic = open_in "data/day3.txt" in
  let content = In_channel.input_all ic in
  let result = find_matched_toggle content true 0 0 in
  close_in ic;
  print_endline (string_of_int result)
