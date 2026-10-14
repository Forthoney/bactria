type t : bits64 & bits64

val width : int
val height : int
val can_play : t -> int -> bool
val play : t -> int -> t
val is_winning_move : t -> int -> bool

val empty : t

val is_full : t -> bool

(* number of moves since the beginning of the game *)
val num_moves : t -> int
