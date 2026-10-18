module type Config = sig
  (* columns to try, best first; must list every column exactly once *)
  val order : int array
end

module type S = sig
  val solve : Bitboard.t -> a:int -> b:int -> int

  (* full window: every score is distinguishable *)
  module Strong : Solver_intf.Strong

  (* null window around 0: only the sign of the score is reliable *)
  module Weak : Solver_intf.Weak
end
