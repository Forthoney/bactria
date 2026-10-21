open! Core
open Bactria
open Unboxed_datatypes

let rec interactive board =
  printf
    "%s\n%s to move [1-%d]: %!"
    (Board.to_string (Bitboard.to_board board))
    (Player.to_string (Bitboard.current_player board))
    Bitboard.width;
  match In_channel.input_line In_channel.stdin with
  | None -> print_endline ""
  | Some line ->
    let retry msg =
      print_endline msg;
      interactive board
    in
    (match Int.of_string_opt line with
     | Some i when i <= Bitboard.width && i > 0 ->
       let col = i - 1 in
       (match Bitboard.play board col with
        | T #(None, _) -> retry (sprintf "column %d is full" i)
        | T #(Some, next) ->
          (match Bitboard.outcome next with
           | Continue -> interactive next
           | (Win _ | Draw) as outcome ->
             printf
               "%s\n%s\n"
               (Board.to_string (Bitboard.to_board next))
               (Player.outcome_to_string outcome)))
     | _ -> retry "enter a single column number")
;;

let command =
  Command.basic
    ~summary:"Play a game interactively"
    (let%map_open.Command () = return () in
     fun () -> interactive Bitboard.empty)
;;
