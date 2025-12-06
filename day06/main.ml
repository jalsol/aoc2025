open Core
open Aoc

let op_func = function
  | '+' -> 0, ( + )
  | '*' -> 1, ( * )
  | _ -> failwith "unreachable"
;;

let apply_op op numbers =
  let init, f = op_func op in
  List.fold numbers ~init ~f
;;

module P1 = struct
  let parse input =
    let grid =
      input
      |> List.map ~f:(fun line ->
        line |> String.split ~on:' ' |> List.filter ~f:(Fn.non String.is_empty))
    in
    let nums = grid |> List.drop_last_exn |> List.transpose_exn in
    let ops = grid |> List.last_exn in
    List.zip_exn nums ops
  ;;

  let calc_col (nums, op) =
    nums |> List.map ~f:Int.of_string |> apply_op (String.get op 0)
  ;;

  let solve input = List.sum (module Int) ~f:calc_col (parse input)
end

module P2 = struct
  let parse_grid lines =
    let max_len =
      List.map lines ~f:String.length |> List.max_elt ~compare |> Option.value_exn
    in
    lines
    |> List.map ~f:(fun line -> String.pad_right line ~len:max_len |> String.to_array)
    |> Array.of_list
  ;;

  let col_chars grid col = Array.map grid ~f:(fun row -> row.(col))
  let is_sep_col grid col = col_chars grid col |> Array.for_all ~f:(Char.equal ' ')

  let number_from_col grid col =
    col_chars grid col
    |> Array.to_list
    |> List.drop_last_exn
    |> List.filter ~f:Char.is_digit
    |> function
    | [] -> None
    | digits -> Some (String.of_char_list digits |> Int.of_string)
  ;;

  let get_op grid col =
    let c = grid.(Array.length grid - 1).(col) in
    Option.some_if (Char.equal c '*' || Char.equal c '+') c
  ;;

  let group_problems grid =
    let num_cols = Array.length grid.(0) in
    List.range 0 num_cols
    |> List.group ~break:(fun a b -> is_sep_col grid a || is_sep_col grid b)
    |> List.filter ~f:(fun group -> not (is_sep_col grid (List.hd_exn group)))
    |> List.map ~f:List.rev
  ;;

  let solve_problem grid cols =
    let numbers = List.filter_map cols ~f:(number_from_col grid) in
    let op = List.find_map cols ~f:(get_op grid) |> Option.value ~default:'+' in
    apply_op op numbers
  ;;

  let solve input =
    let grid = parse_grid input in
    group_problems grid |> List.sum (module Int) ~f:(solve_problem grid)
  ;;
end

let () =
  let input = Utils.raw_lines "day06/input.txt" in
  printf "%d\n" (P1.solve input);
  printf "%d\n" (P2.solve input)
;;
