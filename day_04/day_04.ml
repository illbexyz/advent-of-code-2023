open Base
open Stdio

type card = { id : int; winning_numbers : int list; numbers : int list }
[@@deriving show]

module Parser = struct
  open Angstrom

  let wss = take_while1 Char.is_whitespace
  let num = take_while1 Char.is_digit >>| Int.of_string
  let card_id = string "Card" *> wss *> num <* char ':' <* wss
  let numbers = sep_by1 wss num

  let card =
    (fun id winning_numbers _ numbers -> { id; winning_numbers; numbers })
    <$> card_id <*> numbers
    <*> (wss *> string "|" <* wss)
    <*> numbers

  let cards = sep_by1 end_of_line card
  let run = parse_string cards ~consume:All
end

let n_in_winning_numbers card =
  List.count card.numbers ~f:(fun number ->
      List.mem card.winning_numbers number ~equal:Int.equal)

let () =
  let input = In_channel.read_all "input.txt" in
  let cards = Parser.run input |> Result.ok_or_failwith in
  let part_1 =
    List.map cards ~f:n_in_winning_numbers
    |> List.map ~f:(fun n -> match n with 0 -> 0 | n -> Int.pow 2 (n - 1))
    |> List.fold ~init:0 ~f:( + )
  in
  print_endline ("Part 1: " ^ Int.to_string part_1);

  let part_2 = 0 in
  print_endline ("Part 2: " ^ Int.to_string part_2)
