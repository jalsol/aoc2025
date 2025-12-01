let run input =
  input
  |> List.to_seq
  |> Seq.scan (fun x y -> ((x + y) mod 100 + 100) mod 100) 50
;;

let solve input = run input |> Seq.filter (Int.equal 0) |> Seq.length
