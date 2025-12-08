open Core
open Aoc

module Point = struct
  type t =
    { index : int
    ; coords : int * int * int
    }
  [@@deriving sexp]

  let create index lst =
    let x = List.nth_exn lst 0 in
    let y = List.nth_exn lst 1 in
    let z = List.nth_exn lst 2 in
    { index; coords = x, y, z }
  ;;

  let dist p1 p2 =
    let { coords = x1, y1, z1; _ } = p1 in
    let { coords = x2, y2, z2; _ } = p2 in
    let square x = x * x in
    square (x1 - x2) + square (y1 - y2) + square (z1 - z2)
  ;;
end

module Dsu = struct
  let create len = Array.create ~len (-1)

  let rec root dsu x =
    if dsu.(x) < 0
    then x
    else (
      let par = root dsu dsu.(x) in
      dsu.(x) <- par;
      par)
  ;;

  let unite dsu u v =
    let u = root dsu u in
    let v = root dsu v in
    if u = v
    then false
    else (
      dsu.(u) <- dsu.(u) + dsu.(v);
      dsu.(v) <- u;
      true)
  ;;
end

let input = Utils.raw_lines "day08/input.txt"

let points =
  List.mapi input ~f:(fun i line ->
    line |> String.split ~on:',' |> List.map ~f:Int.of_string |> Point.create i)
;;

let edges =
  let points = Sequence.of_list points in
  Sequence.concat_mapi points ~f:(fun i u ->
    Sequence.drop points (i + 1) |> Sequence.map ~f:(fun v -> u, v, Point.dist u v))
  |> Sequence.to_list
  |> List.sort ~compare:(fun (_, _, d1) (_, _, d2) -> Int.compare d1 d2)
;;

let p1 =
  let n = List.length input in
  let dsu = Dsu.create n in
  List.take edges 1000
  |> List.iter ~f:(fun (p1, p2, _) -> ignore (Dsu.unite dsu p1.index p2.index));
  let vis = Array.create ~len:n false in
  let sizes = ref [] in
  for i = 0 to n - 1 do
    let p = Dsu.root dsu i in
    if not vis.(p)
    then (
      vis.(p) <- true;
      sizes := -dsu.(p) :: !sizes)
  done;
  let sizes = List.sort !sizes ~compare |> List.rev in
  List.nth_exn sizes 0 * List.nth_exn sizes 1 * List.nth_exn sizes 2
;;

let p2 =
  let n = List.length input in
  let dsu = Dsu.create n in
  let final =
    edges
    |> List.fold ~init:(0, None) ~f:(fun (connected, final) (p1, p2, _) ->
      if Option.is_some final || not (Dsu.unite dsu p1.index p2.index)
      then connected, final
      else (
        let final = if connected + 1 = n - 1 then Some (p1, p2) else None in
        connected + 1, final))
    |> snd
  in
  match final with
  | None -> failwith "how?"
  | Some (p1, p2) -> Tuple3.get1 p1.coords * Tuple3.get1 p2.coords
;;

let () =
  printf "%d\n" p1;
  printf "%d\n" p2
;;
