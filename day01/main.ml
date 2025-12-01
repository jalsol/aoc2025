open Core
open Aoc

let parse lines =
  List.map lines ~f:(fun line ->
    let steps = String.slice line 1 0 |> Int.of_string in
    if Char.equal line.[0] 'L' then -steps else steps)

let () =
  let input = Utils.raw_lines "day01/input.txt" |> parse in
  printf "%d\n" (P1.solve input);
  printf "%d\n" (P2.solve (P1.run input) input);