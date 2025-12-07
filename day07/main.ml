open Core
open Aoc

let input = Utils.raw_lines "day07/input.txt" |> Array.of_list
let dimx = Array.length input
let dimy = String.length input.(0)
let start = String.index_exn input.(0) 'S'

let solve () =
  let prev = Array.create ~len:dimy 0 in
  let cur = Array.create ~len:dimy 0 in
  prev.(start) <- 1;
  let p1_ans = ref 0 in
  for i = 1 to dimx - 1 do
    for j = 0 to dimy - 1 do
      if prev.(j) <> 0
      then
        if Char.equal input.(i).[j] '^'
        then (
          incr p1_ans;
          if j + 1 < dimy then cur.(j + 1) <- cur.(j + 1) + prev.(j);
          if j > 0 then cur.(j - 1) <- cur.(j - 1) + prev.(j))
        else cur.(j) <- cur.(j) + prev.(j)
    done;
    for j = 0 to dimy - 1 do
      prev.(j) <- cur.(j);
      cur.(j) <- 0
    done
  done;
  let p2_ans = Array.sum (module Int) prev ~f:Fn.id in
  !p1_ans, p2_ans
;;

let () =
  let p1, p2 = solve () in
  printf "%d\n" p1;
  printf "%d\n" p2
;;
