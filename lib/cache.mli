type t
type key = Bitboard.key

(* a good table size when the caller has no preference *)
val default_size : int
val create : int -> t @ local unique

(* the stored bound for [key], or the loosest possible bound if there is none *)
val find : t @ local -> key -> int64# [@@zero_alloc]
val set : t @ local -> key:key -> data:int64# -> unit [@@zero_alloc]
