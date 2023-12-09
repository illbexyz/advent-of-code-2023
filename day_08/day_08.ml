open Base
open Stdio

(* Allows us to create an infinite iterator over the instructions *)
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
  let label t = t.label
  let left t = t.left
  let right t = t.right
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

  let get_node t label = Hashtbl.find_exn t label
  let get_nodes = Hashtbl.data

  let show_nodes t =
    get_nodes t |> List.map ~f:Node.show |> String.concat ~sep:"\n"

  let get_nodes_ending_with t pattern =
    get_nodes t
    |> List.filter ~f:(fun n -> String.is_suffix n.Node.label ~suffix:pattern)

  let visit t instructions ~from ~to_ =
    let iterator = Iterator.create_repeating instructions in
    let rec aux curr_label steps =
      if String.is_suffix curr_label ~suffix:to_ then steps
      else
        let node = get_node t curr_label in
        let get_fn =
          match Iterator.next iterator with
          | Left -> Node.left
          | Right -> Node.right
        in
        aux (get_fn node) (steps + 1)
    in
    aux from 0

  let rec gcd a b = if b = 0 then a else gcd b (a % b)
  let lcm a b = a * b / gcd a b
  let lcm_list = List.fold ~init:1 ~f:lcm

  let visit_part_2 t instructions ~from ~to_ =
    let starting_nodes = get_nodes_ending_with t from in
    let paths_lengths =
      List.map starting_nodes ~f:(fun node ->
          visit t instructions ~from:(Node.label node) ~to_)
    in
    lcm_list paths_lengths
end

module Parser = struct
  open Angstrom

  let label = take_while1 Char.is_alphanum
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
  let part_1 = Graph.visit graph instructions ~from:"AAA" ~to_:"ZZZ" in
  printf "Part 1: %s\n" @@ Int.to_string part_1;
  let part_2 = Graph.visit_part_2 graph instructions ~from:"A" ~to_:"Z" in
  printf "Part 2: %s\n" @@ Int.to_string part_2
