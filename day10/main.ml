open Core
open Aoc

let parse_mask token =
  String.slice token 1 (-1)
  |> String.foldi ~init:0 ~f:(fun i acc c ->
    if Char.equal c '#' then acc lor (1 lsl i) else acc)
;;

let parse_buttons tokens =
  List.map tokens ~f:(fun token ->
    String.slice token 1 (-1)
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> List.fold ~init:0 ~f:(fun acc i -> acc lor (1 lsl i)))
;;

let parse_freq token =
  String.slice token 1 (-1)
  |> String.split ~on:','
  |> List.to_array
  |> Array.map ~f:Int.of_string
;;

let parse line =
  let tokens = String.split line ~on:' ' in
  let mask = List.hd_exn tokens in
  let tail = List.tl_exn tokens in
  let buttons = List.drop_last_exn tail in
  let freq = List.last_exn tail in
  parse_mask mask, parse_buttons buttons, parse_freq freq
;;

let input = Utils.raw_lines "day10/input.txt" |> List.map ~f:parse
let inf = 1_000_000_000

let bfs_mask (goal, options, _) =
  let dist = Array.create ~len:1024 inf in
  let q = Queue.create () in
  dist.(0) <- 0;
  Queue.enqueue q 0;
  while not (Queue.is_empty q) do
    let u = Queue.dequeue_exn q in
    List.iter options ~f:(fun option ->
      let next = u lxor option in
      if dist.(next) = inf
      then (
        dist.(next) <- dist.(u) + 1;
        Queue.enqueue q next))
  done;
  dist.(goal)
;;

let solve_freq (_, buttons, goal) =
  let n = Array.length goal in
  let m = List.length buttons in
  let buttons_arr = List.to_array buttons in
  let ctx = Z3.mk_context [] in
  let solver = Z3.Optimize.mk_opt ctx in
  let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
  let vars =
    Array.init m ~f:(fun i -> Z3.Arithmetic.Integer.mk_const_s ctx (sprintf "x%d" i))
  in
  Array.iter vars ~f:(fun v ->
    Z3.Arithmetic.mk_ge ctx v zero |> List.return |> Z3.Optimize.add solver);
  for i = 0 to n - 1 do
    let terms =
      Array.filter_mapi vars ~f:(fun j v ->
        Option.some_if (buttons_arr.(j) land (1 lsl i) <> 0) v)
    in
    if Array.length terms > 0
    then
      terms
      |> Array.to_list
      |> Z3.Arithmetic.mk_add ctx
      |> Z3.Boolean.mk_eq ctx (Z3.Arithmetic.Integer.mk_numeral_i ctx goal.(i))
      |> List.return
      |> Z3.Optimize.add solver
    else if goal.(i) <> 0
    then Z3.Optimize.add solver [ Z3.Boolean.mk_false ctx ]
  done;
  vars
  |> Array.to_list
  |> Z3.Arithmetic.mk_add ctx
  |> Z3.Optimize.minimize solver
  |> ignore;
  match Z3.Optimize.check solver with
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Optimize.get_model solver |> Option.value_exn in
    Array.fold vars ~init:0 ~f:(fun acc v ->
      Z3.Model.eval model v true
      |> Option.value_exn
      |> Z3.Arithmetic.Integer.get_big_int
      |> Z.to_int
      |> ( + ) acc)
  | _ -> inf
;;

let p1 = List.sum (module Int) input ~f:bfs_mask
let p2 = List.sum (module Int) input ~f:solve_freq

let () =
  printf "%d\n" p1;
  printf "%d\n" p2
;;
