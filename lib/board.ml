open! Base

type player = Red | Yellow
type slot = player or_null

(* indexed [board.(col).(row)], with row 0 at the bottom *)
type t = slot Array.t Array.t

let width = 7
let height = 6

let slot_to_string = function
  | Null -> "."
  | This Red -> "X"
  | This Yellow -> "O"

let to_string board =
  let row r =
    List.init width ~f:(fun col -> slot_to_string board.(col).(r))
    |> String.concat ~sep:" "
  in
  let rows = List.init height ~f:(fun i -> row (height - 1 - i)) in
  let footer = List.init width ~f:(fun col -> Int.to_string (col + 1)) |> String.concat ~sep:" " in
  String.concat ~sep:"\n" (rows @ [ footer ])
