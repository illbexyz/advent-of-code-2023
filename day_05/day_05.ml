open Base
open Stdio

type range = { dest_start : int; source_start : int; length : int }
[@@deriving show]

type map_categories = { from : string; to_ : string } [@@deriving show]

type map = { categories : map_categories; ranges : range list }
[@@deriving show]

type almanac = { maps : map list; seeds : int list } [@@deriving show]

module Parser = struct
  open Angstrom

  let wss = take_while1 Char.is_whitespace
  let num = take_while1 Char.is_digit >>| Int.of_string
  let label = take_while1 Char.is_alpha
  let seeds = string "seeds:" *> wss *> sep_by (char ' ') num

  let map_categories =
    (fun from to_ -> { from; to_ })
    <$> label <* string "-to-" <*> label <* wss <* string "map:"

  let map_range =
    (fun dest_start source_start length -> { dest_start; source_start; length })
    <$> num <* char ' ' <*> num <* char ' ' <*> num

  let map =
    (fun categories ranges -> { categories; ranges })
    <$> map_categories <* end_of_line
    <*> sep_by1 end_of_line map_range

  let maps = sep_by1 wss map
  let almanac = (fun seeds maps -> { seeds; maps }) <$> seeds <* wss <*> maps
  let run = parse_string almanac ~consume:Prefix
end

let run_map map value =
  let range =
    List.find map.ranges ~f:(fun range ->
        value >= range.source_start && value < range.source_start + range.length)
  in
  match range with
  | None -> value
  | Some range -> range.dest_start + (value - range.source_start)

let rec get_to_location almanac (category : string) (value : int) =
  if String.equal category "location" then value
  else
    let map =
      List.find_exn almanac.maps ~f:(fun map ->
          String.equal map.categories.from category)
    in
    get_to_location almanac map.categories.to_ (run_map map value)

let () =
  let input = In_channel.read_all "input.txt" in
  let almanac = Parser.run input |> Result.ok_or_failwith in
  let part_1 =
    List.map almanac.seeds ~f:(get_to_location almanac "seed")
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in
  print_endline ("Part 1: " ^ Int.to_string part_1);
  let part_2 = 0 in
  print_endline ("Part 2: " ^ Int.to_string part_2)
