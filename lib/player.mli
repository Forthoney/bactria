type t = Red | Yellow
type outcome = Win of t | Draw | Continue

val to_string : t -> string
val outcome_to_string : outcome -> string

(* First player to go *)
val first : t
val other : t -> t
