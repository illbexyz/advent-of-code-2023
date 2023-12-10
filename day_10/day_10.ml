open Base
open Stdio

module Cell = struct
  type connection =
    | NorthSouth
    | WestEast
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
  [@@deriving show]

  type t = Connection of connection | Ground | StartingPosition
  [@@deriving show]

  let from_char = function
    | '|' -> Connection NorthSouth
    | '-' -> Connection WestEast
    | 'L' -> Connection NorthEast
    | 'J' -> Connection NorthWest
    | '7' -> Connection SouthWest
    | 'F' -> Connection SouthEast
    | '.' -> Ground
    | 'S' -> StartingPosition
    | _ -> failwith "Invalid character"
end

module Grid = struct
  type t = { grid : Cell.t array; rows : int; cols : int }

  (* Converts 2d coords to 1d index *)
  let index t x y = (x * t.cols) + y
  let index_to_coords t idx = (idx / t.cols, idx % t.cols)
  let get t (x, y) = Array.get t.grid (index t x y)
  let set t (x, y) v = Array.set t.grid (index t x y) v

  let from_string str ~cell =
    let lines = String.split_lines str in
    let rows = List.length lines in
    let grid =
      List.map lines ~f:(fun line ->
          let chars = String.to_array line in
          Array.map chars ~f:cell)
      |> Array.concat
    in
    let cols = Array.length grid / rows in
    { grid; rows; cols }

  let findi_exn t ~f =
    let idx, item = Array.findi_exn t.grid ~f in
    let x, y = index_to_coords t idx in
    ((x, y), item)

  let get_adjacents_coords t (x, y) ~diagonals =
    let offsets_pos =
      if diagonals then
        let offsets = if diagonals then [ -1; 0; 1 ] else [ -1; 1 ] in
        List.cartesian_product offsets offsets
        |> List.filter ~f:(fun (x, y) ->
               (x <> 0 || y <> 0)
               && x >= 0 && x < t.rows && y >= 0 && y < t.cols)
      else [ (-1, 0); (0, -1); (0, 1); (1, 0) ]
    in
    List.map offsets_pos ~f:(fun (offset_x, offset_y) ->
        (x + offset_x, y + offset_y))

  let get_adjacents t (x, y) ~diagonals =
    get_adjacents_coords t (x, y) ~diagonals
    |> List.map ~f:(fun (x, y) -> ((x, y), get t (x, y)))
end

type direction = North | South | East | West [@@deriving show]
type pos = int * int [@@deriving show]
type step = { from : pos; to_ : pos } [@@deriving show]

let get_step_direction { from; to_ } =
  match (from, to_) with
  | (x1, y1), (x2, y2) when x1 = x2 && y1 < y2 -> East
  | (x1, y1), (x2, y2) when x1 = x2 && y1 > y2 -> West
  | (x1, y1), (x2, y2) when x1 < x2 && y1 = y2 -> South
  | (x1, y1), (x2, y2) when x1 > x2 && y1 = y2 -> North
  | _ -> failwith "Invalid step"

let is_valid_step grid { from; to_ } =
  let direction = get_step_direction { from; to_ } in
  let open Cell in
  match (direction, Grid.get grid to_) with
  | _, StartingPosition -> true
  | (North | South), Connection NorthSouth -> true
  | North, Connection SouthWest -> true
  | North, Connection SouthEast -> true
  | South, Connection NorthEast -> true
  | South, Connection NorthWest -> true
  | (East | West), Connection WestEast -> true
  | East, Connection SouthWest -> true
  | East, Connection NorthWest -> true
  | West, Connection NorthEast -> true
  | West, Connection SouthEast -> true
  | _ -> false

let get_next_step grid prev_step =
  let prev_direction = get_step_direction prev_step in
  let next_direction =
    match (Grid.get grid prev_step.to_, prev_direction) with
    | Cell.Connection NorthSouth, dir -> dir
    | Cell.Connection WestEast, dir -> dir
    | Cell.Connection NorthEast, South -> East
    | Cell.Connection NorthEast, West -> North
    | Cell.Connection NorthWest, South -> West
    | Cell.Connection NorthWest, East -> North
    | Cell.Connection SouthWest, East -> South
    | Cell.Connection SouthWest, North -> West
    | Cell.Connection SouthEast, West -> South
    | Cell.Connection SouthEast, North -> East
    | _ -> failwith "Invalid direction"
  in
  let from = prev_step.to_ in
  let to_ =
    match next_direction with
    | North -> (fst from - 1, snd from)
    | South -> (fst from + 1, snd from)
    | East -> (fst from, snd from + 1)
    | West -> (fst from, snd from - 1)
  in
  { from; to_ }

let get_initial_steps grid =
  let from, _ =
    Grid.findi_exn grid ~f:(fun _idx cell -> phys_equal cell StartingPosition)
  in
  let adj_cells = Grid.get_adjacents grid from ~diagonals:false in
  let valid_adj_steps =
    List.filter_map adj_cells ~f:(fun (to_, _) ->
        let step = { from; to_ } in
        if is_valid_step grid step then Some { from; to_ } else None)
  in
  valid_adj_steps

let walk_loop grid step =
  let step = ref step in
  let length = ref 0 in
  while not @@ phys_equal (Grid.get grid !step.to_) StartingPosition do
    printf "[walk_loop] %s\n" (show_step !step);
    length := !length + 1;
    step := get_next_step grid !step
  done;
  !length

let () =
  let input = In_channel.read_all "input.txt" in
  let grid = Grid.from_string input ~cell:Cell.from_char in
  let initial_steps = get_initial_steps grid in
  let part_1 =
    walk_loop grid (List.hd_exn initial_steps) |> fun length ->
    (length / 2) + (length % 2)
  in
  printf "Part 1: %i\n" part_1;
  let part_2 = 0 in
  printf "Part 2: %i\n" part_2
