module type S = sig
  type t

  val init : unit -> t @ unique
  val start : t -> unit
  val node : t -> unit
end
