open Base
open Stdio

module Parser = struct
  let run contents =
    String.split_lines contents
    |> List.map ~f:(fun line ->
           String.split line ~on:' ' |> List.map ~f:Int.of_string)
end

let rec extrapolate_right history =
  let last_value = List.last_exn history in
  let zipped =
    List.zip_exn (List.tl_exn history) (List.drop_last_exn history)
  in
  let differences = List.map zipped ~f:(fun (a, b) -> a - b) in
  if List.for_all differences ~f:(fun x -> x = 0) then last_value
  else last_value + extrapolate_right differences

let () =
  let input = In_channel.read_all "input.txt" in
  let histories = Parser.run input in
  let extrapolated_values = List.map histories ~f:extrapolate_right in
  let part_1 = List.fold extrapolated_values ~init:0 ~f:( + ) in
  printf "Part 1: %i\n" part_1;
  let part_2 = 0 in
  printf "Part 2: %i\n" part_2
