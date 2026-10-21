open! Base

type t =
  | Red
  | Yellow

type outcome =
  | Win of t
  | Draw
  | Continue

let to_string = function
  | Red -> "Red (X)"
  | Yellow -> "Yellow (O)"
;;

let first = Red

let other = function
  | Red -> Yellow
  | Yellow -> Red
;;

let outcome_to_string = function
  | Continue -> ""
  | Win player -> Printf.sprintf "%s wins!" (to_string player)
  | Draw -> "Draw."
;;
