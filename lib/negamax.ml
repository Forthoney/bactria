open! Base
open! Unboxed
open Bitboard

module Make (C : Negamax_intf.Config) = struct
  (* positions visited since the last top-level [solve] began *)
  let nodes = ref 0

  let rec solve_with (cache @ local) game ~a:alpha ~b:beta =
    Int.incr nodes;
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
              C.order
              ~init:alpha
              ~finish:Fn.id
              ~f:(fun [@inline] alpha col ->
                match play game col with
                | T #(Some, next) ->
                  (* the opponent's window is ours, negated and swapped *)
                  let score = -solve_with cache next ~a:(-beta) ~b:(-alpha) in
                  (* beta cutoff: the opponent would never let us reach this position *)
                  if score >= beta then Stop score else Continue (max alpha score)
                | T #(None, _) -> Continue alpha)
          in
          (* below [beta] means no move caused a cutoff, so [score] is an upper bound *)
          if score < beta then Cache.set cache ~key ~data:(I64.of_int score);
          score)))
  ;;

  let node_count () = !nodes

  (* Searches with [with_cache] if given, which may already hold bounds from earlier
     solves; otherwise with a fresh cache, freed when this returns *)
  let solve ?(with_cache : Cache.t option @ local) game ~a ~b =
    nodes := 0;
    let cache =
      match with_cache with
      | Some cache -> cache
      | None -> Cache.create Cache.default_size
    in
    solve_with cache game ~a ~b [@nontail]
  ;;

  module Strong = struct
    let bound = width * height / 2
    let solve ?with_cache game = solve ?with_cache game ~a:(-bound) ~b:bound
  end

  module Weak = struct
    let solve ?with_cache game : Player.outcome =
      let score = solve ?with_cache game ~a:(-1) ~b:1 in
      let player = Bitboard.current_player game in
      match Ordering.of_int (compare score 0) with
      | Equal -> Draw
      | Greater -> Win player
      | Less -> Win (Player.other player)
    ;;
  end
end

module Center_first = Make (struct
    let order = [| 3; 2; 4; 1; 5; 0; 6 |]
  end)
