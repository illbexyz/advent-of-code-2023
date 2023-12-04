open Base
open Stdio

module Cards = struct
  type t = { id : int; winning_numbers : int list; numbers : int list }
  [@@deriving show]

  let winning_numbers card =
    List.count card.numbers ~f:(fun number ->
        List.mem card.winning_numbers number ~equal:Int.equal)

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

  let from_string = Parser.run
end

let () =
  let input = In_channel.read_all "input.txt" in
  let cards = Cards.from_string input |> Result.ok_or_failwith in
  let part_1 =
    List.map cards ~f:Cards.winning_numbers
    |> List.map ~f:(fun n -> match n with 0 -> 0 | n -> Int.pow 2 (n - 1))
    |> List.fold ~init:0 ~f:( + )
  in
  print_endline ("Part 1: " ^ Int.to_string part_1);

  (* A stack with the ids of the cards to process *)
  let stack = Stack.of_list (List.map cards ~f:(fun card -> card.id)) in
  (* Use an array to have faster random read *)
  let cards_array = Array.of_list cards in
  let number_of_cards = ref 0 in
  Stack.until_empty stack (fun card_id ->
      let card = Array.get cards_array (card_id - 1) in
      number_of_cards := !number_of_cards + 1;
      for x = card.id + 1 to card.id + Cards.winning_numbers card do
        let card_to_add = Array.get cards_array (x - 1) in
        Stack.push stack card_to_add.id
      done);

  let part_2 = !number_of_cards in
  print_endline ("Part 2: " ^ Int.to_string part_2)
