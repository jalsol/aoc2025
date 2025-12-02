open Core
open Aoc

let parse input =
  String.split ~on:',' input
  |> List.map ~f:(fun str ->
    let split = String.split ~on:'-' str in
    Int.of_string (List.nth_exn split 0), Int.of_string (List.nth_exn split 1))
;;

let () =
  let input = List.nth_exn (Utils.raw_lines "day02/input.txt") 0 |> parse in
  printf "%d\n" (P1.solve input);
  printf "%d\n" (P2.solve input)
;;
