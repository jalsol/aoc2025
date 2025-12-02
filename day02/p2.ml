open Core

let is_invalid num =
  let str = Int.to_string num in
  let len = String.length str in
  Sequence.range ~stop:`inclusive 2 len
  |> Sequence.filter ~f:(fun rep -> len mod rep = 0)
  |> Sequence.exists ~f:(fun rep -> P1.is_invalid ~rep num)
;;

let solve = P1.run ~f:is_invalid
