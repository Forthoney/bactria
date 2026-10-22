(* A very straightforward implementation of a connect 4 board, intended to serve as a
   reference implementation We purposefully avoid any optimizations *)
type slot = Player.t or_null

type t =
  { board : slot Array.t Array.t
  ; mutable moves : int
  }

include Board_intf.S with type t := t

val to_string : t -> string
