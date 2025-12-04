open Core
open Aoc

let is_roll map (x, y) = Char.equal map.(x).(y) '@'

let count_adj map x y =
  let height = Array.length map in
  let width = Array.length map.(0) in
  let top = max 0 (x - 1) in
  let bot = min (height - 1) (x + 1) in
  let left = max 0 (y - 1) in
  let right = min (width - 1) (y + 1) in
  Sequence.cartesian_product
    (Sequence.range ~stop:`inclusive top bot)
    (Sequence.range ~stop:`inclusive left right)
  |> Sequence.filter ~f:(is_roll map)
  |> Sequence.length
;;

(* can be improved by keeping a persistent set of current rolls and modify it each call *)
let accessible map =
  let height = Array.length map in
  let width = Array.length map.(0) in
  Sequence.cartesian_product (Sequence.range 0 height) (Sequence.range 0 width)
  |> Sequence.filter ~f:(fun (x, y) -> is_roll map (x, y) && count_adj map x y <= 4)
  |> Sequence.to_list
;;

let p1 map = accessible map |> List.length

let p2 map =
  let rec aux acc =
    let eliminates = accessible map in
    let inc = List.length eliminates in
    if inc = 0
    then acc
    else (
      List.iter eliminates ~f:(fun (x, y) -> map.(x).(y) <- '.');
      aux (acc + inc))
  in
  aux 0
;;

let () =
  let input =
    Utils.raw_lines "day04/input.txt" |> Array.of_list |> Array.map ~f:String.to_array
  in
  printf "%d\n" (p1 input);
  printf "%d\n" (p2 input)
;;
