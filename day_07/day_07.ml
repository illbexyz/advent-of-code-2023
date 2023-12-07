open Base
open Stdio

module Card = struct
  type t = { kind : char } [@@deriving show]

  let from_char str = { kind = str }

  let value ~is_joker card =
    match card with
    | { kind = 'A' } -> 14
    | { kind = 'K' } -> 13
    | { kind = 'Q' } -> 12
    | { kind = 'J' } -> if is_joker then 1 else 11
    | { kind = 'T' } -> 10
    | { kind = '2' .. '9' } -> card.kind |> Char.to_string |> Int.of_string
    | _ -> 0

  let compare a b ~is_joker =
    Int.compare (value a ~is_joker) (value b ~is_joker)
end

module Hand = struct
  type t = { cards : Card.t list } [@@deriving show]

  type type_ =
    | High_card
    | One_pair
    | Two_pair
    | Three_of_a_kind
    | Full_house
    | Four_of_a_kind
    | Five_of_a_kind
  [@@deriving show]

  let from_string str =
    let cards = str |> String.to_list |> List.map ~f:Card.from_char in
    { cards }

  let show t =
    List.map t.cards ~f:(fun card -> card.kind) |> String.of_char_list

  let get_type t ~is_joker =
    let cards =
      if is_joker then
        List.filter t.cards ~f:(fun card -> not (Char.equal card.kind 'J'))
      else t.cards
    in
    let sorted = List.sort ~compare:(Card.compare ~is_joker) cards in
    let grouped =
      List.group sorted ~break:(fun a b -> Card.compare a b ~is_joker <> 0)
    in
    let counts = List.map grouped ~f:(fun group -> List.length group) in
    let max_count =
      List.max_elt counts ~compare:Int.compare |> Option.value ~default:0
    in
    match
      max_count
      +
      if is_joker then
        let num_of_jokers =
          List.count t.cards ~f:(fun card -> Char.equal card.kind 'J')
        in
        num_of_jokers
      else 0
    with
    | 5 -> Five_of_a_kind
    | 4 -> Four_of_a_kind
    | 3 -> if List.length counts = 2 then Full_house else Three_of_a_kind
    | 2 -> if List.length counts = 3 then Two_pair else One_pair
    | _ -> High_card

  let compare a b ~is_joker =
    match Stdlib.compare (get_type a ~is_joker) (get_type b ~is_joker) with
    | 0 ->
        let zipped = List.zip_exn a.cards b.cards in
        let compared =
          List.map zipped ~f:(fun (a, b) -> Card.compare a b ~is_joker)
        in
        List.find compared ~f:(fun v -> v <> 0) |> Option.value ~default:0
    | v -> v
end

module Bet = struct
  type t = { hand : Hand.t; bet : int } [@@deriving show]

  let from_string str =
    match String.split str ~on:' ' with
    | [ hand; bet ] -> { hand = Hand.from_string hand; bet = Int.of_string bet }
    | _ -> failwith "Invalid bet"
end

module Bets = struct
  type t = { bets : Bet.t list } [@@deriving show]

  let from_string str =
    String.split_lines str |> List.map ~f:Bet.from_string |> fun bets ->
    { bets }

  let total_winnings t ~is_joker =
    let sorted =
      List.sort t.bets ~compare:(fun a b ->
          Hand.compare a.hand b.hand ~is_joker)
    in
    let ranks = List.mapi sorted ~f:(fun i bet -> bet.bet * (i + 1)) in
    List.fold ranks ~init:0 ~f:( + )
end

let () =
  let input = In_channel.read_all "input.txt" in
  let bets = Bets.from_string input in
  let part_1 = Bets.total_winnings ~is_joker:false bets in
  printf "Part 1: %s\n" @@ Int.to_string part_1;
  let part_2 = Bets.total_winnings ~is_joker:true bets in
  printf "Part 2: %s\n" @@ Int.to_string part_2
