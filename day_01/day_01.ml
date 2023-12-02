open Base
open Stdio

module Result_syntax = struct
  let ( let* ) x f = Result.bind ~f x
  let return = Result.return
end

module Parser = struct
  open Angstrom

  let num = take_while1 Char.is_digit
  let chars = take_while Char.is_alpha

  let numbers_of_line =
    (fun numbers ->
      List.hd_exn numbers ^ List.last_exn numbers |> Int.of_string)
    <$> many (chars *> num <* chars)

  let lines = sep_by end_of_line numbers_of_line
  let run contents = parse_string ~consume:Consume.All lines contents
end

let part_1_res =
  let open Result_syntax in
  let* input = Parser.run (In_channel.read_all "test.txt") in
  List.fold input ~init:0 ~f:( + ) |> return

let () =
  match part_1_res with
  | Ok result -> print_endline (Int.to_string result)
  | Error err -> print_endline err
