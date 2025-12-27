let get_tbl_added_pair tbl pair =
  match Hashtbl.find_opt tbl (fst pair) with
  | Some x ->
      Hashtbl.replace tbl (fst pair) (snd pair :: x);
      tbl
  | None ->
      Hashtbl.add tbl (fst pair) [ snd pair ];
      tbl

let rec hashtbl_of_seq_pairs tbl seq =
  match seq with
  | [] -> tbl
  | h :: t -> hashtbl_of_seq_pairs (get_tbl_added_pair tbl h) t

let list_list_of_list lst =
  List.map
    (fun s ->
      Scanf.sscanf s "%d|%d" (fun x y -> (string_of_int x, string_of_int y)))
    lst

let is_rule text =
  match Scanf.sscanf_opt text "%d|%d" (fun x _ -> x) with
  | Some _ -> true
  | None -> false

let rec is_correct lst rules =
  match lst with
  | [] -> true
  | h :: t -> (
      match Hashtbl.find_opt rules h with
      | Some next ->
          if List.exists (fun x -> List.mem x t) next then false
          else is_correct t rules
      | None -> is_correct t rules)

let print_hashtable tbl =
  Hashtbl.iter
    (fun key value ->
      Printf.printf "%s -> [" key;
      List.iter (Printf.printf "%s; ") value;
      print_endline "]")
    tbl

let print_string_list_list lst =
  List.iter
    (fun inner_list ->
      Printf.printf "[";
      List.iter (fun s -> Printf.printf "%s; " s) inner_list;
      Printf.printf "]\n")
    lst

let get_middle lst = List.nth lst (List.length lst / 2)
let rec acc_list acc = function [] -> acc | h :: t -> acc_list (acc + h) t

let part1 () =
  let ic = open_in "data/day5.txt" in
  let content = In_channel.input_all ic in
  close_in ic;

  let lines =
    List.filter (fun s -> s <> "") (String.split_on_char '\n' content)
  in
  let rules, updates = List.partition (fun s -> is_rule s) lines in
  let rules_tbl =
    hashtbl_of_seq_pairs (Hashtbl.create 0) (list_list_of_list rules)
  in
  let updates_nums = List.map (fun x -> String.split_on_char ',' x) updates in
  let correct_lines =
    List.filter (fun line -> is_correct (List.rev line) rules_tbl) updates_nums
  in
  let middles = List.map (fun x -> get_middle x) correct_lines in
  let result = acc_list 0 (List.map (fun x -> int_of_string x) middles) in
  print_endline (string_of_int result)

let corrected_line lst rules =
  let comparator x y =
    match Hashtbl.find_opt rules x with
    | Some next -> if List.mem y next then 1 else 0
    | None -> 0
  in
  List.sort comparator lst

let part2 () =
  let ic = open_in "data/day5.txt" in
  let content = In_channel.input_all ic in
  close_in ic;
  let lines =
    List.filter (fun s -> s <> "") (String.split_on_char '\n' content)
  in
  let rules, updates = List.partition (fun s -> is_rule s) lines in
  let rules_tbl =
    hashtbl_of_seq_pairs (Hashtbl.create 0) (list_list_of_list rules)
  in
  let updates_nums = List.map (fun x -> String.split_on_char ',' x) updates in

  let bad_lines =
    List.filter
      (fun line -> not (is_correct (List.rev line) rules_tbl))
      updates_nums
  in
  let correct_lines =
    List.map
      (fun line -> List.rev (corrected_line (List.rev line) rules_tbl))
      bad_lines
  in
  let middles = List.map (fun x -> get_middle x) correct_lines in
  let result = acc_list 0 (List.map (fun x -> int_of_string x) middles) in
  print_endline (string_of_int result)
