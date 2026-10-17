open Bitboard

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
        (* Without forcing inline, search becomes an heap allocated closure *)
        let [@inline] [@loop] rec search col alpha =
          if col = width then alpha
          else
            match play game col with
            | T #(Some, next) ->
              (* the opponent's window is ours, negated and swapped *)
              let score = -solve next ~a:(-beta) ~b:(-alpha) in
              (* beta cutoff: the opponent would never let us reach this position *)
              if score >= beta then score
              else search (col + 1) (max alpha score)
            | T #(None, _) -> search (col + 1) alpha
        in
        search 0 alpha
      
