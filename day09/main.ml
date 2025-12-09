open Core
open Aoc

let poly =
  Utils.raw_lines "day09/input.txt"
  |> List.map ~f:(fun line ->
    let nums = String.split ~on:',' line |> List.map ~f:Int.of_string in
    List.nth_exn nums 0, List.nth_exn nums 1)
;;

let area (x1, y1) (x2, y2) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

let max_area_by pred =
  List.concat_mapi poly ~f:(fun i p1 ->
    List.drop poly (i + 1)
    |> List.filter_map ~f:(fun p2 -> Option.some_if (pred p1 p2) (area p1 p2)))
  |> List.fold ~init:0 ~f:max
;;

let h_edges, v_edges =
  let n = List.length poly in
  poly
  |> List.mapi ~f:(fun i p1 -> p1, List.nth_exn poly ((i + 1) mod n))
  |> List.partition_map ~f:(fun ((x1, y1), (x2, y2)) ->
    if y1 = y2
    then First ((min x1 x2, max x1 x2), y1)
    else Second (x1, (min y1 y2, max y1 y2)))
;;

let inside x y =
  let on_h =
    List.exists h_edges ~f:(fun ((x1, x2), ey) ->
      y = ey && Int.between x ~low:x1 ~high:x2)
  in
  let on_v =
    List.exists v_edges ~f:(fun (ex, (y1, y2)) ->
      x = ex && Int.between y ~low:y1 ~high:y2)
  in
  let crossings =
    List.count v_edges ~f:(fun (ex, (y1, y2)) ->
      ex > x && Int.between y ~low:y1 ~high:(y2 - 1))
  in
  on_h || on_v || crossings % 2 = 1
;;

let h_seg_valid x1 x2 y =
  let lo = min x1 x2 in
  let hi = max x1 x2 in
  let cut =
    List.exists v_edges ~f:(fun (ex, (y1, y2)) ->
      Int.between ex ~low:(lo + 1) ~high:(hi - 1)
      && Int.between y ~low:(y1 + 1) ~high:(y2 - 1))
  in
  (not cut) && inside ((lo + hi) / 2) y
;;

let v_seg_valid x y1 y2 =
  let lo = min y1 y2 in
  let hi = max y1 y2 in
  let cut =
    List.exists h_edges ~f:(fun ((x1, x2), ey) ->
      Int.between ey ~low:(lo + 1) ~high:(hi - 1)
      && Int.between x ~low:(x1 + 1) ~high:(x2 - 1))
  in
  (not cut) && inside x ((lo + hi) / 2)
;;

let rect_valid (x1, y1) (x2, y2) =
  let lx = min x1 x2 in
  let hx = max x1 x2 in
  let ly = min y1 y2 in
  let hy = max y1 y2 in
  h_seg_valid lx hx ly
  && h_seg_valid lx hx hy
  && v_seg_valid lx ly hy
  && v_seg_valid hx ly hy
;;

let p1 = max_area_by (fun _ _ -> true)
let p2 = max_area_by rect_valid

let () =
  printf "%d\n" p1;
  printf "%d\n" p2
;;
