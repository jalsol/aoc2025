open Core

let parse lines =
  List.map lines ~f:(fun line ->
    let steps = String.slice line 1 0 |> Int.of_string in
    if Char.equal line.[0] 'L' then -steps else steps)
