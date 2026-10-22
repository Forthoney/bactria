open! Base
open! Unboxed
open Unboxed_datatypes
open Bitboard

(* Board X is a child position of Board Y if we can make one move in Y to get X Here, we
   store all 7 moves at once in two 256bit SIMD registers: [low] and [high] We take
   advantage of the fact that all the children share the same pos component, and thus we
   only need to store the nonempty component.

   [low]: children from playing columns 0-3 and [high]: children from playing columns 4-6

   We "waste" the last lane of high. NOTE: when AVX-512 support is added, we would be able
   to do this with a single AVX-512 "register" *)
module Ix4 = Ocaml_simd_avx.Int64x4

module Children = struct
  type t =
    #{ pos : i64
     ; low : Ix4.t
     ; high : Ix4.t
     }

  (* columns 0-3 *)
  let bot_low = Ix4.const #1L #128L #16384L #2097152L

  (* columns 4-7 *)
  let bot_high = Ix4.const #268435456L #34359738368L #4398046511104L #562949953421312L

  (* [play] with every lane playing a different column: pos' = pos lxor nonempty (same in
     every lane) nonempty' = nonempty lor (nonempty + bot) (the carry trick: see play) *)
  let[@inline] of_board (#{ pos; nonempty } : Bitboard.t) =
    let nonempty_v = Ix4.set1 nonempty in
    #{ pos = I64.O.(pos lxor nonempty)
     ; low = Ix4.(nonempty_v lor (nonempty_v + bot_low))
     ; high = Ix4.(nonempty_v lor (nonempty_v + bot_high))
     }
  ;;

  (* Bit [c] of the result is set iff playing column [c] is legal. Lane 3 of [high] is the
     nonexistent column 7, so only 3 of its lanes are kept; the two halves are then joined
     into bits 0-6. *)
  let[@inline] legal #{ pos = _; low; high } : i64 =
    let[@inline] legal_lanes nonempty ~bot =
      let sentinel = Ix4.shifti_left_logical #6L (* height *) bot in
      Ix4.(movemask (nonempty land sentinel = zero))
    in
    I64.O.(
      legal_lanes low ~bot:bot_low
      lor ((legal_lanes high ~bot:bot_high land #7L (* columns 4-6 *))
           lsl 4 (* after [low] *)))
  ;;

  (* Bit [i] of the result is set iff lane [i] makes four in a row for the player who made
     the move. Meaningless for illegal lanes.

     In a child, [pos] is the opponent (now to move), so [pos lxor nonempty] is the stones
     of the player who just moved, including the stone just dropped. For the shift trick
     see [has_four]; here all four directions are computed and OR-ed so a single
     [movemask] finishes the job. The shift amounts must be literals, so each direction is
     written out rather than looped: horizontal 7 / 14, diagonal 6 / 12, diagonal 8 / 16,
     vertical 1 / 2 *)
  let[@inline] winning_lanes pos nonempty =
    let p = Ix4.(set1 pos lxor nonempty) in
    let m = Ix4.(p land shifti_right_logical #7L p) in
    let horizontal = Ix4.(m land shifti_right_logical #14L m) in
    let m = Ix4.(p land shifti_right_logical #6L p) in
    let diag1 = Ix4.(m land shifti_right_logical #12L m) in
    let m = Ix4.(p land shifti_right_logical #8L p) in
    let diag2 = Ix4.(m land shifti_right_logical #16L m) in
    let m = Ix4.(p land shifti_right_logical #1L p) in
    let vertical = Ix4.(m land shifti_right_logical #2L m) in
    let any = Ix4.(horizontal lor diag1 lor diag2 lor vertical) in
    Ix4.(movemask (any <> zero))
  ;;

  (* Bit [c] of the result is set iff playing column [c] is legal and wins. *)
  let[@inline] winning (#{ pos; low; high } as children) =
    let winning =
      I64.O.(winning_lanes pos low lor (winning_lanes pos high lsl 4 (* after [low] *)))
    in
    I64.O.(winning land legal children)
  ;;

  (* Bit [i] of the result is set iff, in the board [stones] belongs to, a stone of theirs
     on cell [i] would complete four in a row.

     Each lane handles one direction (vertical 1, horizontal 7, diagonals 6 and 8), using
     the per-lane variable shifts. The vertical lane also checks lines that continue above
     the cell, which [threats] then reports for cells with a stone above them; those are
     never empty, so no cell the opponent can play is affected. Two shuffles OR the
     directions together, leaving the result in every lane. *)
  let[@inline] threats stones =
    let p = Ix4.set1 stones in
    (* lanes: vertical, horizontal, diag-falling, diag-rising *)
    let s1 = Ix4.const #1L #7L #6L #8L (* one step *) in
    let s2 = Ix4.const #2L #14L #12L #16L (* two steps *) in
    let s3 = Ix4.const #3L #21L #18L #24L (* three steps *) in
    let[@inline] shl s = Ix4.shift_left_logical_by p ~shift:s in
    let[@inline] shr s = Ix4.shift_right_logical_by p ~shift:s in
    let lower = Ix4.(shl s1 land shl s2) in
    let higher = Ix4.(shr s1 land shr s2) in
    let r =
      Ix4.(
        lower
        land shl s3
        lor (lower land shr s1)
        lor (higher land shr s3)
        lor (higher land shl s1))
    in
    let r = Ix4.(r lor permute [%permute 2, 3, 0, 1] (* swap halves *) r) in
    Ix4.(r lor permute_lanes [%permute (1, 0), (1, 0)] (* swap neighbours *) r)
  ;;

  (* Bit [c] of the result is set iff playing column [c] lets the opponent win on the very next move. *)
  let[@inline] losing #{ pos; low; high } =
    let threats = threats pos in
    let bot = Ix4.const1 #4432676798593L (* bottom row *) in
    let board = Ix4.const1 #279258638311359L (* all cells *) in
    let open Ix4 in
    let[@inline] losing_lanes nonempty =
      let playable = (nonempty + bot) land board in
      movemask (playable land threats <> zero)
    in
    I64.O.(losing_lanes low lor (losing_lanes high lsl 4 (* after [low] *)))
  ;;

  (* The board after playing column [col]; [col] must be legal *)
  let[@inline] get #{ pos; low; high } col : Bitboard.t =
    let col = I64.of_int col in
    let nonempty =
      if I64.(col < #4L (* [low] lanes *))
      then Ix4.extract ~idx:col low
      else Ix4.extract ~idx:I64.(col - #4L (* [low] lanes *)) high
    in
    #{ pos; nonempty }
  ;;
end

let order = [| 3; 2; 4; 1; 5; 0; 6 |] (* center first *)

let solve ?(with_cache : Cache.t option @ local) (module Stat : Stat_intf.S) game =
  let cache =
    match with_cache with
    | Some cache -> cache
    | None -> Cache.create Cache.default_size
  in
  let stat = Stat.init () in
  let rec loop game ~a:(alpha : i64) ~b:(beta : i64) : i64 =
    Stat.node stat;
    if is_full game
    then #0L
    else (
      let children = Children.of_board game in
      let moves = num_moves game in
      if I64.O.(Children.winning children <> zero)
      then I64.of_int (((width * height) + 1 - moves) / 2)
      else (
        let moves_left =
          I64.O.(Children.legal children land lnot (Children.losing children))
        in
        (* every move lets the opponent win on their next move *)
        if I64.O.(moves_left = zero)
        then I64.of_int (-((width * height) - moves) / 2)
        else if moves >= (width * height) - 2 (* last two *)
        then
          (* we cannot lose on the next move nor win on this one, and at most two stones
             are left, so no one can win *)
          #0L
        else (
          (* the opponent cannot win on their next move, so the worst we can do is lose on
             their move after that *)
          let lower = I64.of_int (-((width * height) - 2 - moves) / 2) in
          let alpha = I64.max alpha lower in
          if I64.O.(alpha >= beta)
          then alpha
          else (
            let key = Bitboard.key game in
            let upper = I64.of_int (((width * height) - 1 - moves) / 2) in
            (* an earlier search of this position may have found a tighter bound *)
            let upper = I64.min upper (Cache.find cache key) in
            let beta = I64.min beta upper in
            if I64.O.(alpha >= beta)
            then beta
            else (
              let[@inline] [@loop] rec search i alpha =
                if i = Array.length order
                then alpha
                else if I64.O.(moves_left land (#1L lsl order.(i)) = zero)
                then search (i + 1) alpha
                else (
                  let child = Children.get children order.(i) in
                  let score = I64.O.(-loop child ~a:(-beta) ~b:(-alpha)) in
                  if I64.O.(score >= beta)
                  then score
                  else search (i + 1) (I64.max alpha score))
              in
              let score = search 0 alpha in
              if I64.O.(score < beta) then Cache.set cache ~key ~data:score;
              score)))))
  in
  (* the score is in [min, max]: narrow it with null-window searches until they meet *)
  let rec window min max =
    let open I64.O in
    if min >= max
    then min
    else (
      let med = min + ((max - min) / #2L) in
      let med =
        if med <= zero && min / #2L < med
        then min / #2L
        else if med >= zero && max / #2L > med
        then max / #2L
        else med
      in
      let score = loop game ~a:med ~b:(med + #1L) in
      if score <= med then window min score else window score max)
  in
  let size = I64.of_int Bitboard.size in
  let moves = I64.of_int (Bitboard.num_moves game) in
  Stat.start stat;
  let min = I64.O.(-(size - moves) / #2L) in
  let max = I64.O.((size + #1L - moves) / #2L) in
  let score = window min max in
  Stat.finish stat;
  I64.to_int_exn score
;;
