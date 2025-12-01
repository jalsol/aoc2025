let crossings acc cur =
  if cur > 0 then
    (acc + cur) / 100
  else if cur < 0 then
    ((100 - acc) mod 100 - cur) / 100
  else
    0
;;

let solve pos step =
  Seq.zip pos (List.to_seq step)
  |> Seq.fold_left (fun total (acc, cur) -> total + crossings acc cur) 0
;;

