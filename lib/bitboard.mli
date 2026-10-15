open Unboxed_datatypes

(* We use an immutable board representation since this representation is
   compact enough to be "trivially" copyable *)
type t : bits64 & bits64

val width : int
val height : int
val play : t -> int -> (t Option_u.t[@kind bits64 & bits64])

val is_winning_move : t -> int -> bool

val empty : t

val is_full : t -> bool

(* number of moves since the beginning of the game *)
val num_moves : t -> int
(* the player whose turn it is; Red moves first *)
val current_player : t -> Player.t

val to_board : t -> Board.t
