let int_list_of_string line =
  List.map int_of_string (Str.split (Str.regexp_string " ") line)

let increasing_condition current last = current > last && current <= last + 3
let decreasing_condition current last = current < last && current >= last - 3

let rec is_safe lst last_num condition =
  match lst with
  | [] -> true
  | h :: t -> if condition h last_num then is_safe t h condition else false

let part1 () =
  let ic = open_in "data/day2.txt" in
  let rec read_lines sum =
    try
      let line = input_line ic in
      let report = int_list_of_string line in
      let is_safe =
        is_safe (List.tl report) (List.hd report) increasing_condition
        || is_safe (List.tl report) (List.hd report) decreasing_condition
      in
      if is_safe then read_lines (sum + 1) else read_lines sum
    with End_of_file ->
      close_in ic;
      sum
  in
  print_endline (string_of_int (read_lines 0))

let rec is_safe_skip lst last_num skipped condition =
  match lst with
  | [] -> true
  | h :: t ->
      if condition h last_num then
        let non_skip = is_safe_skip t h skipped condition in
        if not skipped then
          let skip = is_safe_skip t last_num true condition in
          skip || non_skip
        else non_skip
      else if not skipped then is_safe_skip t last_num true condition
      else false

let part2 () =
  let ic = open_in "data/day2.txt" in
  let rec read_lines sum =
    try
      let line = input_line ic in
      let report = int_list_of_string line in
      let tail = List.tl report in
      if
        is_safe (List.tl report) (List.hd report) increasing_condition
        || is_safe (List.tl report) (List.hd report) decreasing_condition
      then read_lines (sum + 1)
      else if
        is_safe_skip (List.tl report) (List.hd report) false
          increasing_condition
        || is_safe_skip (List.tl report) (List.hd report) false
             decreasing_condition
      then read_lines (sum + 1)
      else if
        is_safe_skip (List.tl tail) (List.hd tail) true increasing_condition
        || is_safe_skip (List.tl tail) (List.hd tail) true decreasing_condition
      then read_lines (sum + 1)
      else read_lines sum
    with End_of_file ->
      close_in ic;
      sum
  in
  print_endline (string_of_int (read_lines 0))
