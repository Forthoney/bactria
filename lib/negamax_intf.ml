module type Config = sig
  (* columns to try, best first; must list every column exactly once *)
  val order : int array
end

module type S = sig
  (* searches with [with_cache] if given, else with a fresh [Cache.default_size] cache *)
  val solve : ?with_cache:Cache.t @ local -> Bitboard.t -> a:int -> b:int -> int

  (* positions visited by the most recent solve (through [solve], [Strong] or [Weak]) *)
  val node_count : unit -> int

  (* full window: every score is distinguishable *)
  module Strong : Solver_intf.Strong

  (* null window around 0: only the sign of the score is reliable *)
  module Weak : Solver_intf.Weak
end
