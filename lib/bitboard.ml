open! Base
open! Unboxed
open Unboxed_datatypes

type t = #{pos : i64; nonempty : i64}

let width = 7
let height = 6

let empty = #{pos = #0L; nonempty = #0L}

let key #{pos; nonempty} =
  I64.O.(pos + nonempty)

let col_shift col = col * (height + 1)

let top_mask col =
  I64.O.((#1L lsl Int.(height - 1)) lsl col_shift col)

let bot_mask col =
  I64.O.(#1L lsl col_shift col)

let can_play #{pos; nonempty} col =
  0 <= col && col < width && I64.O.(nonempty land top_mask col = zero)

let [@zero_alloc] play (#{pos; nonempty} as game) col =
  if can_play game col then
    (Option_u.some [@kind bits64 & bits64])
      I64.O.(#{pos = pos lxor nonempty; nonempty = nonempty lor (nonempty + bot_mask col)})
  else (Option_u.none [@kind bits64 & bits64]) ()

let [@zero_alloc] num_moves #{pos = _; nonempty} =
  I64.to_int_exn (I64.popcount nonempty)

(* four in a row along the direction whose neighbouring cells are [shift] bits apart *)
let has_four pos shift =
  let shift2 = 2 * shift in
  let open I64.O in
  let m = pos land (pos lsr shift) in
  m land (m lsr shift2) <> zero

let is_aligned pos =
  has_four pos (height + 1)    (* horizontal *)
  || has_four pos height       (* diagonal 1 *)
  || has_four pos (height + 2) (* diagonal 2 *)
  || has_four pos 1            (* vertical *)

(* [pos] holds the stones of the player to move, and [pos lxor nonempty] those
   of the player who just moved *)
let [@zero_alloc] is_won #{pos; nonempty} =
  is_aligned pos || is_aligned I64.O.(pos lxor nonempty)

let is_full board =
  num_moves board = width * height

(* Red moves first *)
let current_player board =
  if num_moves board % 2 = 0 then Player.Red else Player.Yellow

let to_board board : Board.t =
  let #{pos; nonempty} = board in
  let to_move = current_player board in
  let other = Player.other to_move in
  let board = 
    Array.init width ~f:(fun col ->
      Array.init height ~f:(fun row ->
        let bit = I64.O.(#1L lsl Int.(col_shift col + row)) in
        if I64.O.(nonempty land bit = zero) then Null
        else if I64.O.(pos land bit = zero) then This other
        else This to_move)) in
  {board = board; turn = to_move}

let [@zero_alloc] rec negamax game alpha beta =
  if is_full game then 0
  else
    let [@loop] rec has_winning_move col =
      col < width
      &&
      match play game col with
      | T #(Some, next) when is_won next -> true
      | _ -> has_winning_move (col + 1)
    in
    if has_winning_move 0 then (width * height + 1 - num_moves game) / 2
    else
      (* we cannot win this move, so the best we can hope for is winning next move *)
      let upper = (width * height - 1 - num_moves game) / 2 in
      let beta = min beta upper in
      if alpha >= beta then beta
      else
        (* Without forcing inline, search becomes an heap allocated closure *)
        let [@inline] [@loop] rec search col alpha =
          if col = width then alpha
          else
            match play game col with
            | T #(Some, next) ->
              let score = -negamax next (-beta) (-alpha) in
              if score >= beta then score
              else search (col + 1) (max alpha score)
            | T #(None, _) -> search (col + 1) alpha
        in
        search 0 alpha
