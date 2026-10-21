open! Core
open Bactria

let solve_lines =
  In_channel.iter_lines ~f:(fun line ->
    let board = Bitboard.init_with line in
    if Bitboard.is_won board then invalid_arg (sprintf "game is already over: %s" line);
    Negamax_simd.solve (module Stat_nop) board
    |> Int.to_string
    |> Out_channel.print_endline)
;;

let command =
  Command.basic
    ~summary:"Solve the board starting from a given sequence of moves"
    ~readme:(fun () ->
      "The input must be 1-based column numbers, e.g. 4453.\n\
       It is assumed to be a valid sequence of moves.\n\
       The moves must be separated by newlines.\n")
    (let%map_open.Command file =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       match file with
       | "-" -> solve_lines In_channel.stdin
       | file -> In_channel.with_file file ~f:solve_lines)
;;
