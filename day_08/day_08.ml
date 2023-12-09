open Base
open Stdio

(* Allows us to create an infinite iteratore over the instructions  *)
module Iterator = struct
  type 'a t = { list : 'a list; sequence : 'a Sequence.t; next : unit -> 'a }

  let create_repeating list =
    let sequence = ref (Sequence.cycle_list_exn list) in
    let next () =
      let item, next_sequence = Sequence.next !sequence |> Option.value_exn in
      sequence := next_sequence;
      item
    in
    { list; sequence = !sequence; next }

  let next t = t.next ()
end

module Node = struct
  type t = { label : string; left : string; right : string } [@@deriving show]

  let create label left right = { label; left; right }
end

type instruction = Left | Right

module Graph = struct
  type t = (string, Node.t) Hashtbl.t

  let create : Node.t list -> t =
   fun nodes ->
    let table = Hashtbl.create (module String) in
    List.iter nodes ~f:(fun n ->
        Hashtbl.add_exn table ~key:n.Node.label ~data:n);
    table

  let get_node = Hashtbl.find_exn

  let visit t instructions curr_label =
    let rec aux t instructions curr_label steps =
      if String.equal curr_label "ZZZ" then steps
      else
        let node = get_node t curr_label in
        let instruction = Iterator.next instructions in
        (* printf "Visiting %s\n" node.Node.label; *)
        match instruction with
        | Left -> aux t instructions node.Node.left (steps + 1)
        | Right -> aux t instructions node.Node.right (steps + 1)
    in
    aux t instructions curr_label 0
end

module Parser = struct
  open Angstrom

  let label = take_while1 Char.is_alpha
  let wss = take_while1 Char.is_whitespace

  let instructions =
    label >>| String.to_list
    >>| List.map ~f:(function
          | 'L' -> Left
          | 'R' -> Right
          | _ -> failwith "Invalid instruction")

  let node =
    Node.create <$> label
    <*> string " = (" *> label
    <* string ", " <*> label <* char ')'

  let graph = Graph.create <$> sep_by1 end_of_line node

  let problem =
    (fun instructions graph -> (instructions, graph))
    <$> instructions <* wss <*> graph

  let run = parse_string problem ~consume:All
end

let () =
  let input = In_channel.read_all "input.txt" in
  let instructions, graph = Parser.run input |> Result.ok_or_failwith in
  let part_1 =
    Graph.visit graph (Iterator.create_repeating instructions) "AAA"
  in
  printf "Part 1: %s\n" @@ Int.to_string part_1;
  let part_2 = 0 in
  printf "Part 2: %s\n" @@ Int.to_string part_2
