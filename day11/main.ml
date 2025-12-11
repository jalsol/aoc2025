open Core
open Aoc

let parse_line line =
  let line = String.split line ~on:' ' in
  let src = String.slice (List.hd_exn line) 0 (-1) |> String.hash in
  let adj = List.tl_exn line |> List.map ~f:String.hash in
  src, adj
;;

let adj_list =
  let adj_list = Hashtbl.create (module Int) in
  Utils.raw_lines "day11/input.txt"
  |> List.map ~f:parse_line
  |> List.iter ~f:(fun (src, adj) -> Hashtbl.set adj_list ~key:src ~data:adj);
  adj_list
;;

let out = String.hash "out"
let you = String.hash "you"
let svr = String.hash "svr"
let dac = String.hash "dac"
let fft = String.hash "fft"
let dac_mask = 1
let fft_mask = 2

let dfs ~target_mask ~src =
  let dp = Hashtbl.create (module Int) in
  let rec aux u =
    if Option.is_some (Hashtbl.find dp u)
    then ()
    else if u = out
    then
      Hashtbl.set dp ~key:u ~data:(Array.init 4 ~f:(fun i -> Bool.to_int (Int.equal 0 i)))
    else (
      let adj = Hashtbl.find_exn adj_list u in
      List.iter adj ~f:aux;
      let is_dac = dac_mask * Bool.to_int (u = dac) in
      let is_fft = fft_mask * Bool.to_int (u = fft) in
      let toggle_mask = is_dac lor is_fft in
      let value = Array.create ~len:4 0 in
      List.iter adj ~f:(fun v ->
        let prev_value = Hashtbl.find_exn dp v in
        for mask = 0 to 3 do
          let next_mask = mask lor toggle_mask in
          value.(next_mask) <- value.(next_mask) + prev_value.(mask)
        done);
      Hashtbl.set dp ~key:u ~data:value)
  in
  aux src;
  let value = Hashtbl.find_exn dp src in
  match target_mask with
  | Some goal -> value.(goal)
  | None -> Array.sum (module Int) value ~f:Fn.id
;;

let p1 = dfs ~target_mask:None ~src:you
let p2 = dfs ~target_mask:(Some 3) ~src:svr

let () =
  printf "%d\n" p1;
  printf "%d\n" p2
;;
