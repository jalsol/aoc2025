open Core
open Aoc

let power = Array.init 12 ~f:(Int.pow 10)
let ascii_0 = Char.to_int '0'

let calc input ~d =
  let len = String.length input in
  let dp = Array.make_matrix ~dimx:(len + 2) ~dimy:d 0 in
  for i = len downto 1 do
    let digit = Char.to_int input.[i - 1] - ascii_0 in
    dp.(i).(0) <- max digit dp.(i + 1).(0)
  done;
  for l = 1 to d - 1 do
    for i = len - l downto 1 do
      let digit = Char.to_int input.[i - 1] - ascii_0 in
      dp.(i).(l) <- (digit * power.(l)) + dp.(i + 1).(l - 1);
      dp.(i).(l) <- max dp.(i).(l) dp.(i + 1).(l)
    done
  done;
  dp.(1).(d - 1)
;;

let p1 = List.fold ~init:0 ~f:(fun acc line -> acc + calc line ~d:2)
let p2 = List.fold ~init:0 ~f:(fun acc line -> acc + calc line ~d:12)

let () =
  let input = Utils.raw_lines "day03/input.txt" in
  printf "%d\n" (p1 input);
  printf "%d\n" (p2 input)
;;
