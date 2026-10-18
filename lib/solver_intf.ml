(* exact score: positive means the side to move wins, and the larger the sooner *)
module type Strong = sig
  val solve : Bitboard.t -> int
end

module type Weak = sig
  val solve : Bitboard.t -> Player.outcome
end
