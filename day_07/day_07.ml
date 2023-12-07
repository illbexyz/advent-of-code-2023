open Base
open Stdio

module Card = struct
  type t = { kind : char } [@@deriving show]

  let from_char str = { kind = str }

  let value = function
    | { kind = 'A' } -> 13
    | { kind = 'K' } -> 12
    | { kind = 'Q' } -> 11
    | { kind = 'J' } -> 10
    | { kind = 'T' } -> 9
    | { kind = '9' } -> 8
    | { kind = '8' } -> 7
    | { kind = '7' } -> 6
    | { kind = '6' } -> 5
    | { kind = '5' } -> 4
    | { kind = '4' } -> 3
    | { kind = '3' } -> 2
    | { kind = '2' } -> 1
    | _ -> 0

  let compare a b = Int.compare (value a) (value b)
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

  let get_type t =
    let sorted = List.sort ~compare:Card.compare t.cards in
    let grouped = List.group sorted ~break:(fun a b -> Card.compare a b <> 0) in
    let counts = List.map grouped ~f:(fun group -> List.length group) in
    let max_count = List.max_elt counts ~compare:Int.compare in
    match max_count with
    | Some 5 -> Five_of_a_kind
    | Some 4 -> Four_of_a_kind
    | Some 3 -> if List.length counts = 2 then Full_house else Three_of_a_kind
    | Some 2 -> if List.length counts = 3 then Two_pair else One_pair
    | _ -> High_card

  let compare a b =
    match Stdlib.compare (get_type a) (get_type b) with
    | 0 ->
        let zipped = List.zip_exn a.cards b.cards in
        let compared = List.map zipped ~f:(fun (a, b) -> Card.compare a b) in
        List.find compared ~f:(fun v -> v <> 0) |> Option.value ~default:0
    | v -> v
end

module Bet = struct
  type t = { hand : Hand.t; bet : int } [@@deriving show]

  let from_string str =
    let parts = String.split str ~on:' ' in
    let hand = List.hd_exn parts in
    let bet = List.last_exn parts |> Int.of_string in
    { hand = Hand.from_string hand; bet }
end

module Bets = struct
  type t = { bets : Bet.t list } [@@deriving show]

  let from_string str =
    String.split_lines str |> List.map ~f:Bet.from_string |> fun bets ->
    { bets }

  let total_winnings (t : t) =
    let sorted =
      List.sort t.bets ~compare:(fun a b -> Hand.compare a.hand b.hand)
    in
    let ranks = List.mapi sorted ~f:(fun i bet -> bet.bet * (i + 1)) in
    List.fold ranks ~init:0 ~f:( + )
end

let () =
  let input = In_channel.read_all "input.txt" in
  let bets = Bets.from_string input in
  let part_1 = Bets.total_winnings bets in
  print_endline @@ "Part 1: " ^ Int.to_string part_1;
  let part_2 = 0 in
  print_endline @@ "Part 2: " ^ Int.to_string part_2
