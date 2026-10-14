type player = Red | Yellow
type slot = player or_null
type t = slot Array.t Array.t

val width : int
val height : int

val to_string : t -> string
