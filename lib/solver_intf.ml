(* exact score: positive means the side to move wins, and the larger the sooner *)
(* [with_cache] is a transposition table to search with. It may already hold bounds from
   earlier solves, which then speed this one up. Without it, every call creates a fresh
   [Cache.default_size] table. *)

module type Strong = sig
  val solve : ?with_cache:Cache.t @ local -> Bitboard.t -> int
end

module type Weak = sig
  val solve : ?with_cache:Cache.t @ local -> Bitboard.t -> Player.outcome
end
