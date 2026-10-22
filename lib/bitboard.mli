[@@@zero_alloc all]

(* We use an immutable board representation since this representation is compact enough to
   be "trivially" copyable *)
type t =
  #{ pos : int64#
   ; nonempty : int64#
   }

type key = int64#

include Board_intf.S with type t := t

(* whether either player has four in a row *)
val is_won : t -> bool
val init_with : string -> t @ unique

(* unique identifier for a position; fits in 49 bits *)
val key : t -> key
val to_board : t -> Board.t @ unique [@@zero_alloc ignore]
val outcome : t -> Player.outcome [@@zero_alloc ignore]
