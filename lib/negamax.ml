open! Base
open Bitboard

module Make (C : Negamax_intf.Config) = struct
  let [@zero_alloc] rec solve game ~a:alpha ~b:beta =
    if is_full game then 0
    else
      let [@loop] rec has_winning_move col =
        col < width &&
        match play game col with
        | T #(Some, next) when is_won next -> true
        | _ -> has_winning_move (col + 1)
      in
      if has_winning_move 0 then (width * height + 1 - num_moves game) / 2
      else
        (* we cannot win this move, so the best we can hope for is winning on our next
           move, which is one move (two stones) later than the bound above *)
        let upper = (width * height - 1 - num_moves game) / 2 in
        let beta = min beta upper in
        (* the window is empty: no score we could return would matter *)
        if alpha >= beta then beta
        else
          (* an explicit loop rather than [Array.fold_until]: with [C.order] abstract,
             the generic fold cannot rule out a float array and may allocate *)
          let [@loop] rec search alpha i =
            if i = Array.length C.order then alpha
            else
              match play game C.order.(i) with
              | T #(Some, next) ->
                (* the opponent's window is ours, negated and swapped *)
                let score = -solve next ~a:(-beta) ~b:(-alpha) in
                (* beta cutoff: the opponent would never let us reach this position *)
                if score >= beta then score else search (max alpha score) (i + 1)
              | T #(None, _) -> search alpha (i + 1)
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
