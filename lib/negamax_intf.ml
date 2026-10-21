module type S = sig
  (* searches with [with_cache] if given, else with a fresh [Cache.default_size] cache *)
  val solve : ?with_cache:Cache.t @ local -> (module Stat_intf.S) -> Bitboard.t -> int
end
