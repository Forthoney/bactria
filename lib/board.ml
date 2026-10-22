open! Base

type slot = Player.t or_null

let slot_to_char = function
  | Null -> '.'
  | This Player.Red -> 'X'
  | This Player.Yellow -> 'O'
;;

type t =
  { board : slot array array
  ; mutable moves : int
  }

let width = 7
let height = 6
let size = width * height

let create () =
  { board = [| [| Null for _ = 0 to height - 1 |] for _ = 0 to width - 1 |]; moves = 0 }
;;

let is_full { board; moves = _ } =
  Array.for_all ~f:(Array.for_all ~f:Or_null.is_this) board
;;

let num_moves { board = _; moves } = moves
let current_player game = if num_moves game % 2 = 0 then Player.Red else Player.Yellow

let can_play { board; moves = _ } col =
  0 <= col && col < width && Or_null.is_null board.(col).(height - 1)
;;

let play (game @ unique) col : #(Board_intf.played * t) @ unique =
  if col < 0 || col >= width
  then #(Out_of_bound, game)
  else (
    let played : Board_intf.played =
      let ({ board; moves } as g) = borrow_ game in
      match Array.findi ~f:(fun _ slot -> Or_null.is_null slot) board.(col) with
      | None -> Full
      | Some (row, _) ->
        board.(col).(row) <- This (current_player g);
        g.moves <- moves + 1;
        Legal
    in
    #(played, game))
;;

let to_string { board; moves = _ } =
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
          Char.of_int_exn (Char.to_int '1' + col))
;;
