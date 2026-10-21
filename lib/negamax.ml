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
  let rec loop stat game ~a:alpha ~b:beta =
    Stat.node stat;
    if is_full game
    then 0
    else (
      let[@inline] [@loop] rec has_winning_move col =
        col < width
        &&
        match play game col with
        | T #(Some, next) when is_won next -> true
        | _ -> has_winning_move (col + 1)
      in
      if has_winning_move 0
      then ((width * height) + 1 - num_moves game) / 2
      else (
        (* we cannot win this move, so the best we can hope for is winning on our next
           move, which is one move (two stones) later than the bound above *)
        let key = Bitboard.key game in
        let upper = ((width * height) - 1 - num_moves game) / 2 in
        (* an earlier search of this position may have found a tighter bound *)
        let upper = min upper (I64.to_int_trunc (Cache.find cache key)) in
        let beta = min beta upper in
        (* the window is empty: no score we could return would matter *)
        if alpha >= beta
        then beta
        else (
          let score =
            Array.fold_until
              order
              ~init:alpha
              ~finish:Fn.id
              ~f:(fun [@inline] alpha col ->
                match play game col with
                | T #(Some, next) ->
                  (* the opponent's window is ours, negated and swapped *)
                  let score = -loop stat next ~a:(-beta) ~b:(-alpha) in
                  (* beta cutoff: the opponent would never let us reach this position *)
                  if score >= beta then Stop score else Continue (max alpha score)
                | T #(None, _) -> Continue alpha)
          in
          (* below [beta] means no move caused a cutoff, so [score] is an upper bound *)
          if score < beta then Cache.set cache ~key ~data:(I64.of_int score);
          score)))
  in
  let bound = width * height / 2 in
  Stat.start stat;
  loop stat game ~a:(-bound) ~b:bound [@nontail]
;;
