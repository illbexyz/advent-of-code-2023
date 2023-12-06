open Base
open Stdio

type race = { time : int; distance : int } [@@deriving show]
type race_list = race list [@@deriving show]

module Parser = struct
  open Angstrom

  let wss = take_while Char.is_whitespace
  let num = take_while1 Char.is_digit >>| Int.of_string
  let times = string "Time:" *> wss *> sep_by wss num
  let distances = string "Distance:" *> wss *> sep_by wss num

  let races =
    (fun times distances ->
      List.zip_exn times distances
      |> List.map ~f:(fun (time, distance) -> { time; distance }))
    <$> times <* wss <*> distances <* wss

  let run = parse_string races ~consume:All
end

let () =
  let input = In_channel.read_all "input.txt" in
  let races = Parser.run input |> Result.ok_or_failwith in
  let part_1 =
    List.map races ~f:(fun { time; distance } ->
        List.range 0 time ~stop:`inclusive
        |> List.map ~f:(fun hold_button_ms ->
               let ms_of_movement = time - hold_button_ms in
               let movement = ms_of_movement * hold_button_ms in
               movement)
        |> List.filter ~f:(fun movement -> movement > distance)
        |> List.length)
    |> List.fold ~init:1 ~f:( * )
  in
  printf "Part 1: %i\n" part_1
