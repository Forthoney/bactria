type slot = Player.t or_null
type t = {board : slot Array.t Array.t; mutable turn : Player.t}

val width : int
val height : int
val play : t @ unique -> int -> bool

val make : unit -> t

val is_full : t -> bool

val num_moves : t -> int
val current_player : t -> Player.t

val to_string : t -> string
