open! Base

type slot = Player.t or_null

let slot_to_char = function
  | Null -> '.'
  | This Player.Red -> 'X'
  | This Player.Yellow -> 'O'
;;

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

let to_string ({ board; turn = _ } @ local) =
  let line_length = 2 * width in
  String.init
    (((height + 1) * line_length) - 1)
    ~f:(fun i ->
      let line = i / line_length in
      match i % line_length with
      | c when c = line_length - 1 -> '\n'
      | c when c % 2 = 1 -> ' '
      | c ->
        let col = c / 2 in
        if line < height
        then slot_to_char board.(col).(height - 1 - line)
        else
          (* the footer: one digit per column, so [width] must stay below 10 *)
          Char.of_int_exn (Char.to_int '1' + col)) [@nontail]
;;

let current_player { board = _; turn } = turn
