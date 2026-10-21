open! Base
open! Unboxed

type elem = int64#
type key = Bitboard.key
type t = elem array

let data_bits = 15
let key_bits = 64 - data_bits

(* No score can exceed this, so it is the loosest upper bound there is *)
let loosest = I64.of_int (Bitboard.width * Bitboard.height / 2)
let default_size = 65_521 (* prime, so keys spread evenly; 512 KB *)
let create len = exclave_ Array.create_local ~len #0L

(* The data sits in the top [data_bits] bits as two's complement, so [asr] (not [lsr])
   brings it back down with its sign: negative bounds survive the round trip. *)
let[@inline] splat el =
  let open I64 in
  let key_mask = (#1L lsl key_bits) - #1L in
  #(el land key_mask, el asr key_bits)
;;

let[@inline] pack key data = I64.O.(key lor (data lsl key_bits))

(* An all-zero slot is empty. It is also what the empty board (key 0) with data 0 would
   pack to, so that one entry is never found, which only costs a cache hit. *)
let[@inline] [@zero_alloc] find tbl key =
  let pos = I64.to_int_trunc key % Array.length tbl in
  let el = tbl.(pos) in
  let #(key', value) = splat el in
  if I64.O.(el <> zero) && I64.equal key' key then value else loosest
;;

let[@inline] [@zero_alloc] set tbl ~key:k ~data:d =
  let pos = I64.to_int_trunc k % Array.length tbl in
  tbl.(pos) <- pack k d
;;
