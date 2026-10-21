(* Stats about the solving process *)
module type S = sig
  type t

  val init : unit -> t @ unique

  (* can't annotate these as unique until we get exclusive mutable *)
  val start : t -> unit
  val finish : t -> unit
  val node : t -> unit
end
