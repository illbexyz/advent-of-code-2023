open Base
open Stdio

type part = { x : int; y_start : int; y_end : int; id : string }
type symbol = { x : int; y : int; char : char }
type schematics = { part_numbers : part list; symbols : symbol list }

module Parser = struct
  let create_schematics lines =
    let array = Array.of_list_map lines ~f:(fun row -> String.to_array row) in
    let get t x y =
      let row = Array.get t x in
      let char = Array.get row y in
      char
    in
    let part_numbers = ref [] in
    let symbols = ref [] in
    let current_part_number = ref None in

    let add_symbol x y c = symbols := !symbols @ [ { x; y; char = c } ] in

    let add_current_part_number () =
      match !current_part_number with
      | None -> ()
      | Some part_number ->
          part_numbers := !part_numbers @ [ part_number ];
          current_part_number := None
    in

    let update_current_part_number x y c =
      match !current_part_number with
      | None ->
          let id = String.of_char c in
          current_part_number := Some { x; y_start = y; y_end = y; id }
      | Some part_number ->
          let id = part_number.id ^ String.of_char c in
          current_part_number := Some { part_number with id; y_end = y }
    in

    let rows_count = Array.length array in
    let cols_count = Array.length array.(0) in

    for x = 0 to rows_count - 1 do
      (* Add current_part_number from previous row *)
      add_current_part_number ();
      for y = 0 to cols_count - 1 do
        let c = get array x y in

        if Char.is_digit c then update_current_part_number x y c
        else add_current_part_number ();

        if Char.equal c '.' then () else add_symbol x y c
      done
    done;
    { part_numbers = !part_numbers; symbols = !symbols }
end

let are_part_and_symbol_near ~(part : part) ~symbol =
  symbol.x >= part.x - 1
  && symbol.x <= part.x + 1
  && symbol.y >= part.y_start - 1
  && symbol.y <= part.y_end + 1

let is_part_near_any_symbol schematics part =
  List.exists schematics.symbols ~f:(fun symbol ->
      are_part_and_symbol_near ~part ~symbol)

let get_parts_near_symbol schematics symbol =
  List.filter schematics.part_numbers ~f:(fun part ->
      are_part_and_symbol_near ~part ~symbol)

let get_parts_near_gears t =
  List.filter t.symbols ~f:(fun symbol -> Char.equal '*' symbol.char)
  |> List.map ~f:(get_parts_near_symbol t)
  |> List.filter ~f:(fun parts -> List.length parts > 1)

let () =
  let lines = In_channel.read_lines "input.txt" in
  let schematics = Parser.create_schematics lines in
  let part_1 =
    List.filter schematics.part_numbers ~f:(is_part_near_any_symbol schematics)
    |> List.fold ~init:0 ~f:(fun acc part -> acc + Int.of_string part.id)
  in
  print_endline ("Part 1: " ^ Int.to_string part_1);

  let part_2 =
    get_parts_near_gears schematics
    |> List.map ~f:(fun parts ->
           List.map parts ~f:(fun part -> Int.of_string part.id))
    |> List.map ~f:(fun parts -> List.fold parts ~init:1 ~f:( * ))
    |> List.fold ~init:0 ~f:( + )
  in
  print_endline ("Part 2: " ^ Int.to_string part_2)
