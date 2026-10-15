type t = Red | Yellow
val to_string : t -> string

(* First player to go *)
val first : t
val other : t -> t
