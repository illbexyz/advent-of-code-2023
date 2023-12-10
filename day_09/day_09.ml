open Base
open Stdio

module Parser = struct
  let run contents =
    String.split_lines contents
    |> List.map ~f:(fun line ->
           String.split line ~on:' ' |> List.map ~f:Int.of_string)
end

let rec extrapolate history ~direction =
  let value =
    match direction with
    | `Left -> List.hd_exn history
    | `Right -> List.last_exn history
  in
  let zipped =
    List.zip_exn (List.tl_exn history) (List.drop_last_exn history)
  in
  let differences = List.map zipped ~f:(fun (a, b) -> a - b) in
  if List.for_all differences ~f:(fun x -> x = 0) then value
  else
    let inner_extrapolation = extrapolate differences ~direction in
    match direction with
    | `Left -> value - inner_extrapolation
    | `Right -> value + inner_extrapolation

let () =
  let input = In_channel.read_all "input.txt" in
  let histories = Parser.run input in
  let part_1 =
    List.map histories ~f:(extrapolate ~direction:`Right)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "Part 1: %i\n" part_1;
  let part_2 =
    List.map histories ~f:(extrapolate ~direction:`Left)
    |> List.fold ~init:0 ~f:( + )
  in
  printf "Part 2: %i\n" part_2
