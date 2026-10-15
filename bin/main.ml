open! Core
open Bactria
open Unboxed_datatypes

type outcome = Continue | Win of Player.t | Draw

let outcome_to_string = function
  | Continue -> ""
  | Win player -> sprintf "%s wins!" (Player.to_string player)
  | Draw -> "Draw."

let print_board board = print_endline (Board.to_string (Bitboard.to_board board))

(* columns are written 1-based, as in "4453" *)
let parse_column c =
  match Char.get_digit c with
  | Some d when d >= 1 && d <= Bitboard.width -> Ok (d - 1)
  | _ -> Error (sprintf "invalid column %C, expected 1-%d" c Bitboard.width)

let column_full col = sprintf "column %d is full" (col + 1)

(* must be computed before [col] is played *)
let outcome_of_move board col =
  if Bitboard.is_winning_move board col then Win (Bitboard.current_player board)
  else if Bitboard.num_moves board + 1 = Bitboard.width * Bitboard.height then Draw
  else Continue

let rec interactive board =
  printf "%s\n%s to move [1-%d]: %!"
    (Board.to_string (Bitboard.to_board board))
    (Player.to_string (Bitboard.current_player board))
    Bitboard.width;
  match In_channel.input_line In_channel.stdin with
  | None -> print_endline ""
  | Some line ->
    let move =
      match String.to_list (String.strip line) with
      | [ c ] -> parse_column c
      | _ -> Error "enter a single column number"
    in
    let retry msg =
      print_endline msg;
      interactive board
    in
    match move with
    | Error msg -> retry msg
    | Ok col ->
      match Bitboard.play board col with
      | T #(None, _) -> retry (column_full col)
      | T #(Some, next) ->
        let outcome = outcome_of_move board col in
        match outcome with
        | Continue -> interactive next
        | Win _ | Draw ->
          print_board next;
          print_endline (outcome_to_string outcome)

(* [moves] pairs each move with its 1-based position in the input *)
let rec process board moves =
  match moves with
  | [] ->
    print_board board;
    Ok ()
  | (i, c) :: rest ->
    match parse_column c with
    | Error msg -> Or_error.errorf "move %d: %s" i msg
    | Ok col ->
      match Bitboard.play board col with
      | T #(None, _) -> Or_error.errorf "move %d: %s" i (column_full col)
      | T #(Some, next) ->
        let outcome = outcome_of_move board col in
        match outcome, rest with
        | Continue, _ -> process next rest
        | (Win _ | Draw), [] ->
          print_board next;
          print_endline (outcome_to_string outcome);
          Ok ()
        | (Win _ | Draw), (i, _) :: _ -> Or_error.errorf "move %d: the game is already over" i

let interactive_command =
  Command.basic ~summary:"Play a game interactively"
    (let%map_open.Command () = return () in
     fun () -> interactive Bitboard.empty)

let process_command =
  Command.basic_or_error ~summary:"Print the board after a sequence of moves"
    ~readme:(fun () ->
      "MOVES is a string of 1-based column numbers, e.g. 4453.\n\
       If MOVES is omitted, it is read from stdin.")
    (let%map_open.Command moves = anon (maybe ("MOVES" %: string)) in
     fun () ->
       let moves =
         match moves with
         | Some moves -> moves
         | None -> In_channel.input_all In_channel.stdin
       in
       String.filter moves ~f:(Fn.non Char.is_whitespace)
       |> String.to_list
       |> List.mapi ~f:(fun i c -> i + 1, c)
       |> process Bitboard.empty)

let () =
  Command.group ~summary:"Connect Four"
    [ "interactive", interactive_command; "process", process_command ]
  |> Command_unix.run
