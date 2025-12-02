open Core

let is_invalid ~rep num =
  let str = Int.to_string num in
  let len = String.length str in
  if len mod rep <> 0
  then false
  else (
    let stride = len / rep in
    let pref = String.sub str ~pos:0 ~len:stride in
    Sequence.range stride len ~stride
    |> Sequence.for_all ~f:(fun pos ->
        let slice = String.sub str ~pos ~len:stride in
        String.equal pref slice)
  )
;;

let run ~f input =
  List.fold input ~init:0 ~f:(fun acc (start, finish) ->
    Sequence.range ~stop:`inclusive start finish
    |> Sequence.filter ~f
    |> Sequence.fold ~init:0 ~f:( + )
    |> ( + ) acc)
;;

let solve = run ~f:(is_invalid ~rep:2)
