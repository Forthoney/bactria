open! Base
open! Unboxed

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

let col_mask col =
  I64.O.(((#1L lsl height) - #1L) lsl col_shift col)

let can_play #{pos = _; nonempty} col =
  I64.O.(nonempty land top_mask col = zero)

let play #{pos; nonempty} col =
  I64.O.(#{pos = pos lxor nonempty; nonempty = nonempty lor (nonempty + bot_mask col)})

let num_moves #{pos = _; nonempty} =
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

let is_winning_move #{pos; nonempty} col =
  I64.O.(((nonempty + bot_mask col) land col_mask col) lor pos)
  |> is_aligned

let is_full board =
  num_moves board = width * height
