open Base
open Stdio

let find_first_digit line =
  String.find line ~f:Char.is_digit
  |> Option.value_exn ~message:"There should always be a digit in the line"
  |> Char.to_string

let get_number_of_line line =
  let first_digit = find_first_digit line in
  let last_digit = find_first_digit (String.rev line) in
  first_digit ^ last_digit |> Int.of_string

let () =
  let lines = In_channel.read_lines "input.txt" in
  let numbers = List.map lines ~f:get_number_of_line in
  let part_1 = List.fold numbers ~init:0 ~f:( + ) in
  print_endline ("Part 1: " ^ Int.to_string part_1)
