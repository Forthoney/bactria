open! Base
open! Unboxed
open Bitboard

let order = [| 3; 2; 4; 1; 5; 0; 6 |]

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
      let[@inline] [@loop] rec has_winning_move col =
        col < width
        &&
        match play game col with
        | T #(Some, next) when is_won next -> true
        | _ -> has_winning_move (col + 1)
      in
      let moves = num_moves game in
      if has_winning_move 0
      then I64.of_int (((width * height) + 1 - moves) / 2)
      else (
        (* we cannot win this move, so the best we can hope for is winning on our next
           move, which is one move (two stones) later than the bound above *)
        let key = Bitboard.key game in
        let upper = I64.of_int (((width * height) - 1 - moves) / 2) in
        (* an earlier search of this position may have found a tighter bound *)
        let upper = I64.min upper (Cache.find cache key) in
        let beta = I64.min beta upper in
        (* the window is empty: no score we could return would matter *)
        if I64.O.(alpha >= beta)
        then beta
        else (
          (* tries the columns from [order.(i)] on *)
          let[@inline] [@loop] rec search i alpha =
            if i = Array.length order
            then alpha
            else (
              match play game order.(i) with
              | T #(None, _) -> search (i + 1) alpha
              | T #(Some, next) ->
                (* the opponent's window is ours, negated and swapped *)
                let score = I64.O.(-loop next ~a:(-beta) ~b:(-alpha)) in
                (* beta cutoff: the opponent would never let us reach this position *)
                if I64.O.(score >= beta)
                then score
                else search (i + 1) (I64.max alpha score))
          in
          let score = search 0 alpha in
          (* below [beta] means no move caused a cutoff, so [score] is an upper bound *)
          if I64.O.(score < beta) then Cache.set cache ~key ~data:score;
          score)))
  in
  let bound = I64.of_int (width * height / 2) in
  Stat.start stat;
  let score = loop game ~a:(I64.neg bound) ~b:bound in
  Stat.finish stat;
  I64.to_int_exn score
;;
