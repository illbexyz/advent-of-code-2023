open Base
open Stdio

type cubes = { color : string; num : int }
type set = { cubes : cubes list }
type game = { id : int; sets : set list }
type colors = { red : int; green : int; blue : int }

module Parser = struct
  open Angstrom

  let color = choice [ string "red"; string "green"; string "blue" ]
  let num = take_while1 Char.is_digit >>| Int.of_string
  let cubes = (fun num color -> { num; color }) <$> num <* char ' ' <*> color
  let set = (fun cubes -> { cubes }) <$> sep_by (string ", ") cubes
  let sets = sep_by (string "; ") set
  let game_id = string "Game " *> num <* string ": "
  let game = (fun id sets -> { id; sets }) <$> game_id <*> sets
  let games = sep_by end_of_line game
  let run = parse_string games ~consume:Consume.All
end

let game_total_colors game =
  let hash_tbl = Hashtbl.create (module String) in

  List.iter game.sets ~f:(fun set ->
      List.iter set.cubes ~f:(fun cube ->
          Hashtbl.update hash_tbl cube.color ~f:(function
            | None -> cube.num
            | Some x -> max x cube.num)));

  {
    red = Hashtbl.find hash_tbl "red" |> Option.value ~default:0;
    green = Hashtbl.find hash_tbl "green" |> Option.value ~default:0;
    blue = Hashtbl.find hash_tbl "blue" |> Option.value ~default:0;
  }

let is_game_admissible { red; green; blue }
    ~max_colors:{ red = maxRed; green = maxGreen; blue = maxBlue } =
  red <= maxRed && green <= maxGreen && blue <= maxBlue

let sum_game_ids games =
  List.fold games ~init:0 ~f:(fun acc game -> acc + game.id)

let power_of_set { red; green; blue } = red * green * blue

let () =
  let games =
    Parser.run (In_channel.read_all "input.txt") |> Result.ok_or_failwith
  in
  let max_colors = { red = 12; green = 13; blue = 14 } in

  let part_1 =
    List.filter games ~f:(fun game ->
        is_game_admissible (game_total_colors game) ~max_colors)
    |> sum_game_ids
  in

  print_endline ("Part 1: " ^ Int.to_string part_1);

  let part_2 =
    List.map games ~f:(fun game -> game_total_colors game |> power_of_set)
    |> List.fold ~init:0 ~f:( + )
  in

  print_endline ("Part 2: " ^ Int.to_string part_2)
