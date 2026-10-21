open! Unboxed
open Unboxed_datatypes

[@@@zero_alloc all]

(* We use an immutable board representation since this representation is compact enough to
   be "trivially" copyable *)
type t =
  #{ pos : i64
   ; nonempty : i64
   }

type key = int64#

val width : int
val height : int
val play : t -> int -> (t Option_u.t[@kind bits64 & bits64])

(* whether either player has four in a row *)
val is_won : t -> bool
val empty : t
val init_with : string -> t
val is_full : t -> bool

(* unique identifier for a position; fits in 49 bits *)
val key : t -> key

(* number of moves since the beginning of the game *)
val num_moves : t -> int

(* the player whose turn it is; Red moves first *)
val current_player : t -> Player.t
val to_board : t -> Board.t @ local [@@zero_alloc ignore]
val outcome : t -> Player.outcome [@@zero_alloc ignore]
