open! Base
open! Unboxed
open Unboxed_datatypes

[@@@zero_alloc all]

(* A position is two 64-bit integers. Layout: We store the board in column major order.
   Each column uses 7 bits: 6 bits for data + 1 sentinel bit (always 0)

   The sentinel bits exist so that a stack of stones can never run into the neighbouring
   column: they are never set in [pos], so no line of four can cross a column boundary
   (see [has_four]).

   sentinel . . . . . . . bit 6, 13, 20, ... (always 0 in [pos]) row 5 5 12 19 26 33 40 47
   row 4 4 11 18 25 32 39 46 row 3 3 10 17 24 31 38 45 row 2 2 9 16 23 30 37 44 row 1 1 8
   15 22 29 36 43 row 0 0 7 14 21 28 35 42 c0 c1 c2 c3 c4 c5 c6

   [nonempty] has a bit for every chip. [pos] has a bit for every chip belonging to the
   player to move. *)
type t =
  #{ pos : i64
   ; nonempty : i64
   }

type key = int64#

let width = 7
let height = 6
let empty = #{ pos = #0L; nonempty = #0L }

(* A unique identifier for a position, at most 49 bits. [pos + nonempty] adds one bit just
   above the top stone of each column, which encodes the column heights, and [pos] alone
   then tells whose stones are where. *)
let key #{ pos; nonempty } = I64.O.(pos + nonempty)
let col_shift col = col * (height + 1)

(* the topmost real cell (row 5) of [col] *)
let top_mask col = I64.O.((#1L lsl Int.(height - 1)) lsl col_shift col)

(* the bottom cell (row 0) of [col] *)
let bot_mask col = I64.O.(#1L lsl col_shift col)

(* a column is playable iff its top cell is empty *)
let can_play #{ pos; nonempty } col =
  0 <= col && col < width && I64.O.(nonempty land top_mask col = zero)
;;

(* Drop a stone into [col]. Two tricks:

   1. [pos lxor nonempty] is the opponent's stones. After this move it is the opponent's
      turn, so their stones become the new "player to move" set. The stone we are dropping
      is added to [nonempty] only, which is exactly right: it belongs to the player who is
      no longer to move.

   2. [nonempty + bot_mask col] drops the stone without computing which row it lands in.
      The stones in a column are always a contiguous run of 1s from row 0, e.g. 0b0000111
      for 3 stones. Adding 1 at the bottom makes the carry ripple through that whole run,
      clearing it and setting the first empty cell above: 0b0000111 + 0b0000001 =
      0b0001000. Other columns are untouched because the carry stops at the first 0.
      OR-ing with the old [nonempty] puts the cleared stones back, leaving 0b0001111. *)
let play (#{ pos; nonempty } as game) col =
  if can_play game col
  then
    (Option_u.some [@kind bits64 & bits64])
      I64.O.(
        #{ pos = pos lxor nonempty; nonempty = nonempty lor (nonempty + bot_mask col) })
  else (Option_u.none [@kind bits64 & bits64]) ()
;;

let num_moves #{ pos = _; nonempty } = I64.to_int_trunc (I64.popcount nonempty)

(* Is there a line of four stones in [pos] whose neighbouring cells are [shift] bits
   apart?

   [m = pos land (pos lsr shift)] has bit [i] set iff cells [i] and [i + shift] are both
   stones: a pair. Doing it again with twice the distance, [m land (m lsr (2 * shift))],
   keeps bit [i] iff cells [i, i+shift] and [i+2*shift, i+3*shift] are both pairs, i.e.
   [i], [i+s], [i+2s], [i+3s] are all stones. Every starting cell is tested at once, with
   no loops.

   Why these shifts give real lines on this layout (columns are 7 bits tall): 1 next cell
   up in the same column (vertical) 7 same row, next column (horizontal) 6 (=7-1) next
   column, one row down (diagonal, descending) 8 (=7+1) next column, one row up (diagonal,
   ascending) A line that would wrap around the top of a column must step over a sentinel
   bit, which is never set, so it fails the test. *)
let[@inline] has_four pos shift =
  let shift2 = 2 * shift in
  let open I64.O in
  let m = pos land (pos lsr shift) in
  m land (m lsr shift2) <> zero
;;

let[@inline] is_aligned pos =
  has_four pos (height + 1) (* horizontal *)
  || has_four pos height (* diagonal 1 *)
  || has_four pos (height + 2) (* diagonal 2 *)
  || has_four pos 1 (* vertical *)
;;

(* Has anyone won? We test both players: [pos] is the player to move and
   [pos lxor nonempty] the player who just moved. (Only the latter can have a new line,
   since the game ends on the first one, but checking both is harmless.) *)
let[@inline] is_won #{ pos; nonempty } =
  is_aligned pos || is_aligned I64.O.(pos lxor nonempty)
;;

let is_full board = num_moves board = width * height

(* Red moves first, so the parity of the stone count gives the turn *)
let current_player board = if num_moves board % 2 = 0 then Player.Red else Player.Yellow

let[@zero_alloc ignore] to_board board : Board.t =
  let #{ pos; nonempty } = board in
  let to_move = current_player board in
  let other = Player.other to_move in
  let board =
    Array.init width ~f:(fun [@zero_alloc ignore] col ->
      Array.init height ~f:(fun row ->
        let bit = I64.O.(#1L lsl Int.(col_shift col + row)) in
        if I64.O.(nonempty land bit = zero)
        then Null
        else if I64.O.(pos land bit = zero)
        then This other
        else This to_move))
  in
  { board; turn = to_move }
;;

(* [String.fold] can't be used here: its accumulator must have a single-register layout,
   and [t] is [bits64 & bits64]. *)
let init_with moves =
  let mutable board = empty in
  for i = 0 to String.length moves - 1 do
    let col = Char.to_int moves.[i] - Char.to_int '1' in
    match play board col with
    | T #(Some, board') -> board <- board'
    | T #(None, _) -> invalid_arg (String.of_char moves.[i])
  done;
  board
;;

let[@zero_alloc ignore] outcome board =
  if is_won board
  then Player.Win (Player.other (current_player board))
  else if is_full board
  then Player.Draw
  else Player.Continue
;;
