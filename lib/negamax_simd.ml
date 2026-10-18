open! Base
open! Unboxed
open Unboxed_datatypes
open Bitboard

(* Board X is a child position of Board Y if we can make one move in Y to get X
   Here, we store all 7 moves at once in two 256bit SIMD registers: [low] and [high]
   We take advantage of the fact that all the children share the same pos component,
   and thus we only need to store the nonempty component.

   [low]: children from playing columns 0-3 and
   [high]: children from playing columns 4-6

   We "waste" the last lane of high.
   NOTE: when AVX-512 support is added, we would be able to do this with a single AVX-512 "register" *)
module Ix4 = Ocaml_simd_avx.Int64x4
module Children = struct
  type t = #{pos : i64; low : Ix4.t; high : Ix4.t}

  (* [bot_mask col] (see above) for four columns at once: bit [7 * col]. *)
  let bot_low = Ix4.const #1L #128L #16384L #2097152L
  let bot_high =
    Ix4.const #268435456L #34359738368L #4398046511104L #562949953421312L

  (* [play] with every lane playing a different column:
       pos'      = pos lxor nonempty                  (same in every lane)
       nonempty' = nonempty lor (nonempty + bot)      (the carry trick: see play) *)
  let of_board (#{pos; nonempty} : Bitboard.t) =
    let nonempty_v = Ix4.set1 nonempty in
    #{ pos = I64.O.(pos lxor nonempty)
     ; low = Ix4.(nonempty_v lor (nonempty_v + bot_low))
     ; high = Ix4.(nonempty_v lor (nonempty_v + bot_high)) }

  (* Bit [c] of the result is set iff playing column [c] is legal. Lane 3 of [high]
     is the nonexistent column 7, so only 3 of its lanes are kept; the two halves
     are then joined into bits 0-6. *)
  let legal #{pos = _; low; high} =
    (* Bit [i] of the result is set iff the move in lane [i] is legal.

       A column is full when its 6 cells are all stones, and then the carry in
       [of_board] runs all the way out of the column and sets the sentinel bit above
       row 5. A legal move can never set that bit. [bot lsl 6] is exactly the
       sentinel of each lane's column (bit [7 * col + 6]), so one AND finds the
       overflow. [movemask] gathers the top bit of every 64-bit lane into an
       integer, and comparing with zero first turns "lane is 0" into "all bits 1". *)
    let [@inline] legal_lanes nonempty ~bot =
      let sentinel = Ix4.shifti_left_logical #6L bot in
      Ix4.(movemask ((nonempty land sentinel) = zero))
    in
    I64.O.(
      legal_lanes low ~bot:bot_low lor ((legal_lanes high ~bot:bot_high land #7L) lsl 4))

  (* Bit [i] of the result is set iff lane [i] makes four in a row for the player
     who made the move. Meaningless for illegal lanes.

     In a child, [pos] is the opponent (now to move), so [pos lxor nonempty] is the
     stones of the player who just moved, including the stone just dropped. For the
     shift trick see [has_four]; here all four directions are computed and OR-ed so
     a single [movemask] finishes the job. The shift amounts must be literals, so
     each direction is written out rather than looped:
       horizontal 7 / 14,  diagonal 6 / 12,  diagonal 8 / 16,  vertical 1 / 2 *)
  let [@inline] winning_lanes pos nonempty =
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

  (* Bit [c] of the result is set iff playing column [c] is legal and wins. *)
  let winning (#{pos; low; high} as children) =
    let winning =
      I64.O.(winning_lanes pos low lor (winning_lanes pos high lsl 4))
    in
    I64.O.(winning land legal children)

  (* The board after playing column [col]; [col] must be legal *)
  let get #{pos; low; high} col : Bitboard.t =
    let col = I64.of_int col in
    let nonempty =
      if I64.(col < #4L) then Ix4.extract ~idx:col low
      else Ix4.extract ~idx:I64.(col - #4L) high
    in
    #{pos; nonempty}
end

module Make (C : Negamax_intf.Config) = struct
  let [@zero_alloc] rec solve game ~a:alpha ~b:beta =
    if is_full game then 0
    else
      let children = Children.of_board game in
      let legal = Children.legal children in
      (* if any legal move wins, we are done without recursing *)
      if I64.O.(Children.winning children <> zero) then
        (width * height + 1 - num_moves game) / 2
      else
        let upper = (width * height - 1 - num_moves game) / 2 in
        let beta = min beta upper in
        if alpha >= beta then beta
        else
          (* an explicit loop rather than [Array.fold_until]: see [Negamax] *)
          let [@loop] rec search alpha i =
            if i = Array.length C.order then alpha
            else
              let col = C.order.(i) in
              if I64.O.(legal land (#1L lsl col) = zero) then search alpha (i + 1)
              else
                let score = -solve (Children.get children col) ~a:(-beta) ~b:(-alpha) in
                if score >= beta then score else search (max alpha score) (i + 1)
          in
          search alpha 0

  module Strong = struct
    let bound = width * height / 2
    let solve game = solve game ~a:(-bound) ~b:bound
  end

  module Weak = struct
    let solve game : Player.outcome =
      let score = solve game ~a:(-1) ~b:1 in
      let player = Bitboard.current_player game in
      if score > 0 then Win player
      else if score < 0 then Win (Player.other player)
      else Draw
  end
end

module Center_first = Make (struct let order = [| 3; 2; 4; 1; 5; 0; 6 |] end)
