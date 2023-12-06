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

let get_final_race races =
  List.fold races ~init:{ distance = 0; time = 0 } ~f:(fun acc curr ->
      {
        distance =
          Int.to_string acc.distance ^ Int.to_string curr.distance
          |> Int.of_string;
        time = Int.to_string acc.time ^ Int.to_string curr.time |> Int.of_string;
      })

let process_race race =
  List.range 0 race.time ~stop:`inclusive
  |> List.map ~f:(fun hold_button_ms ->
         let ms_of_movement = race.time - hold_button_ms in
         let movement = ms_of_movement * hold_button_ms in
         movement)
  |> List.filter ~f:(fun movement -> movement > race.distance)
  |> List.length

let () =
  let input = In_channel.read_all "input.txt" in
  let races = Parser.run input |> Result.ok_or_failwith in
  let part_1 = List.map races ~f:process_race |> List.fold ~init:1 ~f:( * ) in
  print_endline @@ "Part 1: " ^ Int.to_string part_1;
  let final_race = get_final_race races in
  let part_2 = process_race final_race in
  print_endline @@ "Part 2: " ^ Int.to_string part_2
