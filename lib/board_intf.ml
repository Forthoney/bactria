type played =
  | Out_of_bound
  | Full
  | Legal

module type S = sig
  type t : any

  val width : int
  val height : int
  val size : int
  val can_play : t -> int -> bool

  (* I really wanted this to be option, but that complicates using play with when t has
     non_value kind *)
  val play : t @ unique -> int -> #(played * t) @ unique
  val create : unit -> t @ unique
  val is_full : t -> bool
  val num_moves : t -> int
  val current_player : t -> Player.t
end
