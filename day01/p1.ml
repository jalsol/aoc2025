open Aoc
open Common
open Core

let solve input =
  let ans, _ =
    List.fold input ~init:(0, 50) ~f:(fun (ans, pos) step ->
      let next_pos = (pos + step + 100) mod 100 in
      if next_pos = 0 then ans + 1, next_pos else ans, next_pos)
  in
  ans
;;

let () = Utils.raw_lines "day01/input.txt" |> parse |> solve |> printf "%d"
