open Core
open Aoc

let parse input =
  let ranges, ingredients = List.split_while input ~f:(String.( <> ) "") in
  let ranges =
    List.map ranges ~f:(fun line ->
      let split = String.split line ~on:'-' in
      Int.of_string (List.nth_exn split 0), Int.of_string (List.nth_exn split 1))
  in
  let ingredients = List.drop ingredients 1 |> List.map ~f:Int.of_string in
  ranges, ingredients
;;

let p1 ranges ingredients =
  List.count ingredients ~f:(fun num ->
    List.exists ranges ~f:(fun (low, high) -> Int.between ~low ~high num))
;;

let p2 ranges =
  ranges
  |> List.sort ~compare:(fun (low1, _) (low2, _) -> low1 - low2)
  |> List.fold ~init:[] ~f:(fun acc (low, high) ->
    match acc with
    | (prev_low, prev_high) :: tail when low <= prev_high + 1 ->
      (prev_low, max prev_high high) :: tail
    | _ -> (low, high) :: acc)
  |> List.sum (module Int) ~f:(fun (low, high) -> high - low + 1)
;;

let () =
  let ranges, ingredients = Utils.raw_lines "day05/input.txt" |> parse in
  printf "%d\n" (p1 ranges ingredients);
  printf "%d\n" (p2 ranges)
;;
