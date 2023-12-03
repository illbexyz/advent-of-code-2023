open Base
open Stdio

let map_letters_to_number = function
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  (* This should already be a number *)
  | x -> x

let get_concatenated_first_and_last_digit line ~regex =
  let _ = Str.search_forward regex line 0 in
  let first = Str.matched_string line in
  let _ = Str.search_backward regex line (String.length line) in
  let last = Str.matched_string line in
  (first |> map_letters_to_number) ^ (last |> map_letters_to_number)
  |> Int.of_string

let sum_numbers = List.fold ~init:0 ~f:( + )

let solve lines ~regex =
  List.map lines ~f:(get_concatenated_first_and_last_digit ~regex)
  |> sum_numbers

let () =
  let lines = In_channel.read_lines "input.txt" in

  let regex_part_1 = Str.regexp "[0-9]" in
  let part_1 = solve lines ~regex:regex_part_1 in

  print_endline ("Part 1: " ^ Int.to_string part_1);

  let regex_part_2 =
    Str.regexp
      "[0-9]\\|one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine"
  in
  let part_2 = solve lines ~regex:regex_part_2 in

  print_endline ("Part 2: " ^ Int.to_string part_2)
