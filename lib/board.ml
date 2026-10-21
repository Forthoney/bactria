open! Base

type slot = Player.t or_null

let slot_to_string = function
  | Null -> "."
  | This Player.Red -> "X"
  | This Player.Yellow -> "O"
;;

(* indexed [board.(col).(row)], with row 0 at the bottom *)
type t =
  { board : slot Array.t Array.t
  ; mutable turn : Player.t
  }

let width = 7
let height = 6

let play ({ board; turn } as game) col =
  match Array.findi ~f:(fun _ slot -> Or_null.is_null slot) board.(col) with
  | None -> false
  | exception Invalid_argument _ -> false
  | Some (row, _) ->
    board.(col).(row) <- This turn;
    game.turn <- Player.other turn;
    true
;;

let make () =
  let board = Array.init width ~f:(fun _ -> Array.create ~len:height Null) in
  { board; turn = Player.Red }
;;

let is_full { board; turn = _ } =
  Array.for_all ~f:(Array.for_all ~f:Or_null.is_this) board
;;

let num_moves { board; turn = _ } =
  Array.sum (module Int) ~f:(Array.count ~f:Or_null.is_this) board
;;

let to_string { board; turn = _ } =
  let row r =
    List.init width ~f:(fun col -> slot_to_string board.(col).(r))
    |> String.concat ~sep:" "
  in
  let rows = List.init height ~f:(fun i -> row (height - 1 - i)) in
  let footer =
    List.init width ~f:(fun col -> Int.to_string (col + 1)) |> String.concat ~sep:" "
  in
  String.concat ~sep:"\n" (rows @ [ footer ])
;;

let current_player { board = _; turn } = turn
